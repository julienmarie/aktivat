defmodule Data.Redis.Server do
  use GenServer.Behaviour

  @check_status 1000



  defrecord State, write_queue: nil, down_queue: nil, server_queue: nil, backup: nil, slave: nil, master: nil, ring: nil, servername: nil, status: :not_connected

  @moduledoc """
  The main module to save and read data to and from ElasticSearch

  """

  def start_link(args) do
    :gen_server.start_link(__MODULE__, args, [])
  end

  def init(args) do
    Architecture.Server.log("Initializing redis servers #{:proplists.get_value(:ring, args)}:#{:proplists.get_value(:servername, args)}")
    :erlang.send_after(@check_status, self(), :dbstatus)
    {:ok, State.new(write_queue: :queue.new, down_queue: :queue.new,  server_queue: :queue.new, servername: :proplists.get_value(:servername, args),ring:  :proplists.get_value(:ring, args))}
  end

  #DEBUG

  

  #SERVER SETUP API

  def configmaster(_state) do 
    [
      ["SLAVEOF", "NO", "ONE"]
    ]
  end

  def configslave(state) do
    {_,[host, port, _, pass, _],_} = state.master
    [
      ["SLAVEOF", "#{host}", "#{port}"],
      ["CONFIG", "SET", "MASTERAUTH", "#{pass}"]
    ]
  end

  def configbackup(state) do
    {_,[host, port, _, pass, _],_} = state.master
    [
      ["SLAVEOF", "#{host}", "#{port}"],
      ["CONFIG", "SET", "MASTERAUTH", "#{pass}"]
    ]
  end

  def handle_cast({:setup, server}, state) do
    {host, port, database, pass, reconnect} = server
    queues = :lists.merge3(:queue.to_list(state.down_queue), :queue.to_list(state.server_queue), [state.backup])
    serverlist = :lists.merge3(queues, [state.slave], [state.master])
    Architecture.Server.log("#{port}")
    already = Enum.any?(serverlist, fn(x) ->
      case x do 
        {_, [shost, sport, sdatabase, spass, sreconnect], _} -> {shost, sport, sdatabase, spass, sreconnect} == {host, port, database, pass, reconnect}
        _ ->  false
      end
    end)
    state = case already do
      true -> state
      false ->
        case :eredis.start_link(host, port, database, pass, reconnect) do
          {:ok, connection} ->  Architecture.Server.log("Adding a new server #{host}:#{port} to #{state.ring}:#{state.servername}")
                                state.update(server_queue: :queue.in({connection, [host, port, database, pass, reconnect], :not_ready}, state.server_queue))
          _                 ->  state
        end
    end
    {:noreply, state}
  end

  def handle_cast({:assign}, state) do
    state = case Enum.member?([state.backup, state.slave, state.master], nil) do
      false   ->  state
      true    ->  assign(state)
    end
    configure_servers(state)
    {:noreply, state}
  end

  def handle_cast({:add_to_down_queue, server}, state) do
    {:noreply, state.update(down_queue: :queue.in(server, state.down_queue))}
  end

  def handle_cast({:remove_from_down_queue, server}, state) do
    case :queue.member(server, state.down_queue) do
      false ->  {:noreply, state}
      true  ->  newqueue = :queue.filter(fn(x) -> x != server end, state.down_queue)
                {:noreply, state.update(server_queue: :queue.in(server, state.server_queue), down_queue: newqueue)}
    end
  end

  def handle_info(:dbstatus, state) do
    state = check_status(state)
    :erlang.send_after(@check_status, self(), :dbstatus)
    :gen_server.cast(self(), {:assign})
    {:noreply, state}
  end

  def assign(state) do
    case :queue.out(state.server_queue) do
      {:empty, _}                   ->  case {state.backup, state.slave, state.master} do
        {nil,     slave,    nil     }         -> state.update(slave: nil, master: slave)
        {backup,  slave,    nil     }         -> state.update(backup: nil, slave: backup, master: slave)
        {backup,  nil,      _master }         -> state.update(backup: nil, slave: backup)
        _                                     -> state 
      end
      {{:value, server}, newqueue}  ->  case {state.backup, state.slave, state.master} do
        {nil,     nil,      nil     }         -> state.update(master: server, server_queue: newqueue)
        {nil,     nil,      _master }         -> state.update(slave: server, server_queue: newqueue)
        {nil,     slave,    nil     }         -> state.update(slave: server, master: slave, server_queue: newqueue)
        {backup,  slave,    nil     }         -> state.update(backup: server, slave: backup, master: slave) 
        {nil,     _slave,   _master }         -> state.update(backup: server, server_queue: newqueue)
        {backup,  nil,      _master }         -> state.update(backup: server, slave: backup, server_queue: newqueue)
        {backup,  nil,      nil     }         -> state.update(backup: nil, master: backup, slave: server, server_queue: newqueue)
        _                                   -> state
      end
    end
  end

  def configure_servers(state) do
    Enum.each([{:backup, state.backup}, {:slave, state.slave}, {:master, state.master}], fn(server) -> configure_server(server, state) end)
  end

  def configure_server({_type, nil}, _state) do
    :ok
  end

  def configure_server({type, {server, _, status}}, state) do
    #Architecture.Server.log(:erlang.apply(binary_to_atom("config#{atom_to_binary(type)}"), state))
    case status do
      :not_ready -> :eredis.qp(server, :erlang.apply(Data.Redis.Server, binary_to_atom("config#{atom_to_binary(type)}"), [state]))
      :ready -> :ok
    end
    :ok
  end


  #CHECK SERVER STATUS




  def check_status(state) do
    master_status = check_master_status(state.master, state)
    slave_status = check_slave_status(state.slave, state)
    backup_status = check_backup_status(state.backup, state)
    check_queue(state.server_queue)
    check_down_queue(state.down_queue)
    state = state.update(master: master_status, slave: slave_status, backup: backup_status)
    state
  end

  def check_master_status(nil, _) do
    nil
  end

  def check_master_status({connection, [host, port, database, pass, reconnect], status}, state) do
    master_status = case :eredis.q(connection, ["INFO", "replication"]) do
      {:ok, answer} ->  parsed_answer = parse_infos(answer)
                        case :proplists.get_value("role", parsed_answer) do
                          "master"  ->  {connection, [host, port, database, pass, reconnect], :ready}
                          _         ->  configure_server({:master, {connection, [host, port, database, pass, reconnect], status}}, state)
                                        {connection, [host, port, database, pass, reconnect], :not_ready}
                        end
      _             ->  :gen_server.cast(self(), {:add_to_down_queue, {connection, [host, port, database, pass, reconnect], :not_ready}})
                        nil
    end
  end

  def check_slave_status(nil, _) do
    nil
  end

  def check_slave_status({connection, [host, port, database, pass, reconnect], status}, state) do
    slave_status = case :eredis.q(connection, ["INFO", "replication"]) do
      {:ok, answer} ->  parsed_answer = parse_infos(answer)
                        case {:proplists.get_value("role", parsed_answer), :proplists.get_value("master_link_status", parsed_answer), :proplists.get_value("master_sync_in_progress", parsed_answer)} do
                          {"slave", "up", "0"} ->   {connection, [host, port, database, pass, reconnect], :ready}
                          _                    ->   configure_server({:slave, {connection, [host, port, database, pass, reconnect], status}}, state)
                                                    {connection, [host, port, database, pass, reconnect], :not_ready}
                        end
      _             ->  :gen_server.cast(self(), {:add_to_down_queue, {connection, [host, port, database, pass, reconnect], :not_ready}})
                        nil
    end
  end

  def check_backup_status(nil, _) do
    nil
  end

  def check_backup_status({connection, [host, port, database, pass, reconnect], status}, state) do
    slave_status = case :eredis.q(connection, ["INFO", "replication"]) do
      {:ok, answer} ->  parsed_answer = parse_infos(answer)
                        case {:proplists.get_value("role", parsed_answer), :proplists.get_value("master_link_status", parsed_answer), :proplists.get_value("master_sync_in_progress", parsed_answer)} do
                          {"slave", "up", "0"} ->   {connection, [host, port, database, pass, reconnect], :ready}
                          _                    ->   configure_server({:backup, {connection, [host, port, database, pass, reconnect], status}}, state)
                                                    {connection, [host, port, database, pass, reconnect], :not_ready}
                        end
      _             ->  :gen_server.cast(self(), {:add_to_down_queue, {connection, [host, port, database, pass, reconnect], :not_ready}})
                        nil
    end
  end

  def check_queue(server_queue) do
    #Architecture.Server.log("Check Queue")
    queue_list = :queue.to_list(server_queue)
    Enum.each(queue_list, fn(server) -> 
      {connection, _, _} = server
      case :eredis.q(connection, ["PING"]) do
        {:ok, answer} ->  :ok
        _             ->  :gen_server.cast(self(), {:add_to_down_queue, server})
                          :ok
      end
    end)
  end

  def check_down_queue(server_queue) do
    #Architecture.Server.log("Check Down Queue")
    queue_list = :queue.to_list(server_queue)
    Enum.each(queue_list, fn(server) ->
      {connection, [host, port, _, _, _], _status} = server
      #Architecture.Server.log("Checking down for #{host}:#{port}") 
      case :eredis.q(connection, ["PING"]) do
        {:ok, answer} ->  :gen_server.cast(self(), {:remove_from_down_queue, server})
                          :ok
        _             ->  :ok
      end                
    end)
  end


  def parse_infos(infos) do
    infos = String.split(infos, "\r\n")
    Enum.filter_map(infos, 
      fn(x) ->  String.contains?(x, ":") end, 
      fn(x) ->  [a,b] = String.split(x, ":")
                {a,b} end
    )
  end












  #SAVE API


  def handle_call({:status}, from, state) do
    {:reply, state, state}
  end
  

  def handle_cast({:save_event, account, property, eventname, v_userid}, _from, state) do
    save_event(account, property, eventname, v_userid, state)
    {:reply, :ok, state}
  end

  def handle_cast({:save_event_value, account, property, eventname, value, v_userid}, _from, state) do
    save_event_value(account, property, eventname, value, v_userid, state)
    {:reply, :ok, state}
  end

  
  def handle_cast({:save_source, account, property, source_name, campaign_name, medium_name, content_name, term_name, v_userid}, _from, state) do
    save_source(account, property, source_name, campaign_name, medium_name, content_name, term_name, v_userid, state)
    {:reply, :ok, state}
  end

  def handle_cast({:save_config, account, property, platform_type, platform_name, browser_name, browser_version, resolution, v_userid}, _from, state) do
    save_config(account, property, platform_type, platform_name, browser_name, browser_version, resolution, v_userid, state)
    {:reply, :ok, state}
  end


  def handle_cast({:write, queries}, state) do
    case state.master do
      {master, _, _}  ->  :eredis.qp(master, queries)
                          {:noreply, state}
      nil             ->  oldqueue = :queue.to_list(state.write_queue)
                          {:noreply, state.update(write_queue: :queue.from_list(:lists.merge(:queue.to_list(state.write_queue), queries)))}
    end             
  end



  def save_event(account, property, eventname, v_userid, state) do
    #GET TIMESTAMP 
    time = :calendar.universal_time
    #GET EVENT ID
    {:ok, eventid} = find_id(account, property, "event", eventname, state)
    #QUERIES
    queries = :lists.merge([
      # ADD BITMAPS
      setbit(account, property, time, "evt:#{eventid}", v_userid),
      setbit(account, property, time, "v_usr:#{v_userid}::events", eventid),
      # ADD INTO LOGS
      add_to_logs(account, property, time, "v_usr:#{v_userid}", "evt:#{eventname}")
      ])
    handle_cast(self(), {:write, queries})
    :ok
  end

  def save_event_value(account, property, eventname, value, v_userid, state) do
    #GET TIMESTAMP 
    time = :calendar.universal_time
    #GET EVENT ID
    {:ok, eventid} = find_id(account, property, "event", eventname, state)

    ######################################################################################################TODO
    #thevalue = case String.to_float(value) do
    #  {int, _} ->


    #QUERIES
    queries = :lists.merge([
      # ADD BITMAPS
      setbit(account, property, time, "evt:#{eventid}", v_userid, value),
      # ADD INTO LOGS
      add_to_logs(account, property, time, "v_usr:#{v_userid}", "evt:#{eventname}::value:#{value}")
      ])
    handle_cast(self(), {:write, queries})
    :ok
  end

  def save_source(account, property, source_name, campaign_name, medium_name, content_name, term_name, v_userid, state) do
    time = :calendar.universal_time
    {:ok, source_id} = find_id(account, property, "source", source_name, state)
    {:ok, campaign_id} = find_id(account, property, "campaign", campaign_name, state)
    {:ok, medium_id} = find_id(account, property, "medium", medium_name, state)
    {:ok, content_id} = find_id(account, property, "content", content_name, state)
    {:ok, term_id} = find_id(account, property, "term", term_name, state)
    queries = :lists.merge([
      setbit(account, property, time, "utm:source:#{source_id}", v_userid),
      setbit(account, property, time, "utm:campaign:#{campaign_id}", v_userid),
      setbit(account, property, time, "utm:medium:#{medium_id}", v_userid),
      setbit(account, property, time, "utm:content:#{content_id}", v_userid),
      setbit(account, property, time, "utm:term:#{term_id}", v_userid),
      setbit(account, property, time, "v_usr:#{v_userid}::utm:source", source_id),
      setbit(account, property, time, "v_usr:#{v_userid}::utm:campaign", campaign_id),
      setbit(account, property, time, "v_usr:#{v_userid}::utm:medium", medium_id),
      setbit(account, property, time, "v_usr:#{v_userid}::utm:content", content_id),
      setbit(account, property, time, "v_usr:#{v_userid}::utm:term", term_id)
    ])
    {master, _, _} = state.master
    handle_cast(self(), {:write, queries})
    :ok
  end

  def save_config(account, property, platform_type, platform_name, browser_name, browser_version, resolution, v_userid, state) do
    time = :calendar.universal_time
    {:ok, platform_type_id} = find_id(account, property, "platform_type", platform_type, state)
    {:ok, platform_id} = find_id(account, property, "platform_name", platform_name, state)
    {:ok, browser_id} = find_id(account, property, "browser_name", browser_name, state)
    {:ok, browser_version_id} = find_id(account, property, "browser_version", browser_version, state)
    {:ok, resolution_id} = find_id(account, property, "resolution", resolution, state)
    queries = :lists.merge([
      setbit(account, property, time, "config:platform_type:#{platform_type_id}", v_userid),
      setbit(account, property, time, "config:platform:#{platform_id}", v_userid),
      setbit(account, property, time, "config:browser:#{browser_id}", v_userid),
      setbit(account, property, time, "config:browser_version:#{browser_version_id}", v_userid),
      setbit(account, property, time, "config:resolution:#{resolution_id}", v_userid),

      setbit(account, property, time, "v_usr:#{v_userid}::config:platform_type}", platform_type_id),
      setbit(account, property, time, "v_usr:#{v_userid}::config:platform", platform_id),
      setbit(account, property, time, "v_usr:#{v_userid}::config:browser", browser_id),
      setbit(account, property, time, "v_usr:#{v_userid}::config:browser_version}", browser_version_id),
      setbit(account, property, time, "v_usr:#{v_userid}::config:resolution", resolution_id)
    ])
    handle_cast(self(), {:write, queries})
    :ok
  end


  def find_id(account, property, category, item, state) do
    ## HAVE TO REWRITE FOR INDEX AND COUNTER CATEGORIES
    case :eredis.q(state.writeconnection, ["ZSCORE", "akt-tracker::#{account}::#{property}::index::#{category}", item]) do
      {:ok, :undefined} ->  {:ok, id} = :eredis.q(state.writeconnection, ["INCR", "akt-tracker::#{account}::#{property}::counter::#{category}"])
                            :eredis.q(state.writeconnection, ["ZADD", "akt-tracker::#{account}::#{property}::index::#{category}", id, item])
                            {:ok, id}
      {:ok, id} -> {:ok, id} end
  end

  def setbit(account, property, time, key, item) do
    {{y,m,d},{h,mm,_}} = time
    [
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::#{y}-#{m}-#{d}-#{h}", item, "1"],
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::#{y}-#{m}-#{d}", item, "1"],
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::#{y}-#{m}", item, "1"],
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::#{y}", item, "1"],
      ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}", item, "1"]
    ]
  end

  def setbit(account, property, time, key, item, value) do
    {{y,m,d},{h,mm,_}} = time
    case is_number(value) do
      true ->
        dec_value = Tools.Maths.round(value/10)
        cent_value = Tools.Maths.round(value/100)
        thous_value = Tools.Maths.round(value/1000)
        [
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}-#{h}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}", item, "1"],

          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}::#{y}-#{m}-#{d}-#{h}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}::#{y}-#{m}-#{d}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}::#{y}-#{m}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}::#{y}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-dec:#{dec_value}", item, "1"],

          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}::#{y}-#{m}-#{d}-#{h}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}::#{y}-#{m}-#{d}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}::#{y}-#{m}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}::#{y}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-cent:#{cent_value}", item, "1"],

          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}::#{y}-#{m}-#{d}-#{h}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}::#{y}-#{m}-#{d}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}::#{y}-#{m}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}::#{y}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-num-thous:#{thous_value}", item, "1"]
        ]
      false ->
        [
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}-#{h}-#{mm}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}-#{h}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}-#{d}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}-#{m}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}::#{y}", item, "1"],
          ["SETBIT", "akt-tracker::#{account}::#{property}::bit::#{key}::val-original:#{value}", item, "1"]
        ]
      end
  end

  def incr_counter(account, property, time, category, key) do
    {{y,m,d},{h,mm,_}} = time
    [
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}-#{h}-#{mm}", 1],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}-#{h}", 1],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}", 1],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}", 1],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}}", 1]
    ]
  end

  def incr_counter(account, property, time, category, key, value) do
    {{y,m,d},{h,mm,_}} = time
    [
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}-#{h}-#{mm}", value],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}-#{h}", value],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}-#{d}", value],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}-#{m}", value],
      ["HINCRBY", "akt-tracker::#{account}::#{property}::counter::#{category}::#{key}", "#{y}}", value]
    ]
  end

  def add_to_logs(account, property, time, key, value) do
    {{y,m,d},{h,mm,ss}} = time
    [
      ["LPUSH", "akt-tracker::#{account}::#{property}::logs::#{key}", "#{y}-#{m}-#{d}-#{h}-#{mm}-#{ss} #{value}"]
    ]
  end


end