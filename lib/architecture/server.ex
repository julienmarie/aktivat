defmodule Architecture.Server do
  use GenServer.Behaviour

  defrecord State, hostaddress: nil, main_connection: nil, pubsub_connection: nil, connections: [], processes: nil, nodes: nil, connected: false, rings: [], dbnodes: []

  @ping_delay 500

  @moduledoc """
  This module autodiscovers nodes and manage distributed processes

  """

  def reg(name, pid) do
    :gen_server.call(:architecture_server, {:register_process, name, pid, atom_to_binary(node())})
  end

  def freg(name, pid) do
    :gen_server.call(:architecture_server, {:force_register_process, name, pid, atom_to_binary(node())})
  end

  def findp(name) do
    :gen_server.call(:architecture_server, {:find_process, name})
  end

  def status() do
    :gen_server.call(:architecture_server, :status)
  end

  def add_main_db(host, port, pass, reconnect) do
    :gen_server.cast(:architecture_server, {:add_master_db, host, port, pass, reconnect})
  end

  def propagate_db(ring, node, server, used_servernames) do
    :gen_server.cast(:architecture_server, {:propagate_db, ring, node, server, used_servernames})
  end


  def log(message) do
    :gen_server.cast(:architecture_server, {:log, message})
  end


  ##########

  def start_link() do
    :gen_server.start_link({ :local, :architecture_server }, __MODULE__, [], [])
  end

  def init(_) do
    :global.register_name(binary_to_atom("architecture-server-#{atom_to_binary(node())}"), self)
    nodes = HashSet.new()
    {:ok, ip } = :inet.getif 
    {{ipa,ipb,ipc,ipd},_,_} = hd(:lists.flatten(ip))
    {:ok, [[port]]} = :init.get_argument(:trackrport)
    state = State.new(processes: HashDict.new(), nodes: HashSet.put(nodes, atom_to_binary(node())), hostaddress: "#{ipa}.#{ipb}.#{ipc}.#{ipd}:#{list_to_integer(port)}")
    :hash_ring.start_link
    
    {ok, temp_config} = :yaml.load_file("config.yml", [:implicit_atoms])
    config = :erlson.from_nested_proplist( :lists.flatten(temp_config))
    case :erlson.get_value([:redis, :main], config) do
      :undefined -> :ok
      [[ {_, connectionargs }]]  -> 
        host = String.to_char_list!(:proplists.get_value(:host, connectionargs))
        port = :proplists.get_value(:port, connectionargs)
        database = :proplists.get_value(:database, connectionargs)
        pass = case :proplists.get_value(:pass, connectionargs) do
          nil -> []
          other -> String.to_char_list!(other) end
        reconnect = :proplists.get_value(:reconnect, connectionargs)
        :gen_server.cast(:architecture_server, {:add_master_db, host, port, pass, reconnect})
    end
    :erlang.send_after(@ping_delay, self(), :pingnodes)
    
    { :ok, state }
  end



  def handle_call({:add_key, ring, key}, _from, state) do
    case state.main_connection do
      nil -> {:reply, {:error, :nodb}, state}
      _ ->
            case :hash_ring.find_node(ring, key) do
              {:ok, thenode} -> :eredis.q(state.main_connection, ["SET", "trackr::ring:#{ring}::key:#{key}", thenode])
                                {:reply, {:ok, thenode}, state}
              _              -> {:reply, {:error, :other}, state}
            end
    
    end
  end

  def handle_call({:find_node, ring, key}, _from, state) do
    case state.main_connection do
      nil -> {:reply, {:error, :nodb}, state}
      _ ->
              case :eredis.q(state.main_connection, ["GET", "trackr::ring:#{ring}::key:#{key}"]) do
                {:ok, :undefined} ->  case :hash_ring.find_node(ring, key) do
                                        {:ok, thenode} -> :eredis.q(state.main_connection, ["SET", "trackr::ring:#{ring}::key:#{key}", thenode])
                                                          {:reply, {:ok, thenode}, state}
                                        _    ->           {:reply, {:error, :other}, state}
                                      end
                                      
                {:ok, value}      ->  {:reply, {:ok, value}, state}
              end
    end
  end

  def handle_call({:register_process, name, pid, thenode}, _from, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "register_process::#{name}"])
    case HashDict.has_key?(state.processes, name) do
      false ->  Enum.map(state.nodes, fn(othernode) -> 
                  :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:add_process, name, pid, thenode}) end 
                )
                :eredis.q(state.main_connection, ["PUBLISH", "nodes", "register_process::#{name}::SUCCESS"])
                {:reply, :ok, state.update(processes: HashDict.put(state.processes, name, {pid, thenode}))}
      true ->   :eredis.q(state.main_connection, ["PUBLISH", "nodes", "register_process::#{name}::taken"])
                {:reply, :taken, state}
    end  
  end

  def handle_call({:force_register_process, name, pid, thenode}, _from, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "force-register_process::#{name}"])
      Enum.map(state.nodes, fn(othernode) -> 
        :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:add_process, name, pid, thenode}) end 
      )
      :eredis.q(state.main_connection, ["PUBLISH", "nodes", "force-register_process::#{name}::SUCCESS"])
      {:reply, :ok, state.update(processes: HashDict.put(state.processes, name, {pid, thenode}))}  
  end


  def handle_call({:find_process, name}, _from, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "find_process::#{name}"])
    case HashDict.get(state.processes, name) do
      nil             ->    
        :eredis.q(state.main_connection, ["PUBLISH", "nodes", "find_process::#{name}::not_found:not_in_list"])
        {:reply, :not_found, state}
      {theprocess_pid, thenode}  -> 
        isprocessalive = case thenode == atom_to_binary(node()) do
          true -> :erlang.process_info(theprocess_pid)
          false -> 
            case :net_adm.ping binary_to_atom(thenode) do
              :pong -> :gen_server.call(:global.whereis_name(binary_to_atom("architecture-server-#{thenode}")), {:process_info, theprocess_pid})
              :pang -> :undefined
            end
        end
        case  isprocessalive do
          :undefined  ->  Enum.map(state.nodes, fn(othernode) -> 
                            :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:remove_process, name}) end 
                          )
                          :eredis.q(state.main_connection, ["PUBLISH", "nodes", "find_process::#{name}::not_found:dead"])
                          {:reply, :not_found, state.update(processes: HashDict.delete(state.processes, name))}
          _           ->  
                          :eredis.q(state.main_connection, ["PUBLISH", "nodes", "find_process::#{name}::FOUND"])
                          {:reply, theprocess_pid, state}
        end
    end
  end

  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:process_info, theprocess_pid}, _from, state) do
    {:reply, :erlang.process_info(theprocess_pid), state}
  end

  def handle_cast({:add_node, ring, thenode}, state) do
    :hash_ring.add_node(ring, thenode)
    {:noreply, state.update(dbnodes: Enum.uniq([{thenode, ring}| state.dbnodes]))}
  end

  def handle_cast({:create_ring, ring}, state) do
    :hash_ring.create_ring(ring, 1)
    {:noreply, state.update(rings: [ring| state.rings])}
  end

  
  def handle_cast({:delete_node, ring, thenode}, state) do
    :hash_ring.remove_node(ring, thenode)
    {:noreply, state}
  end


  def handle_cast({:propagate_db, ring, node, server, used_servernames},state) do
    Enum.map(state.nodes, fn(othernode) -> 
      :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:receive_db, ring, node, server, List.flatten(Enum.uniq(used_servernames))}) end 
    )
   


    {:noreply, state}
  end

  def handle_cast({:receive_db, ring, node, server, used_servernames}, state) do
    Data.Weissmuller.receive_db(ring, node, server, List.flatten(Enum.uniq(used_servernames)))
    {:noreply, state}
  end

  def handle_cast({:add_process, name, pid, thenode}, state) do
    {:noreply, state.update(processes: HashDict.put(state.processes, name, {pid, thenode}))}
  end 

  def handle_cast({:remove_process, name}, state) do
    Enum.map(state.nodes, fn(othernode) -> 
      :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:remove_the_process, name}) end 
    )
    {:noreply, state}
  end 

  def handle_cast({:remove_the_process, name}, state) do
    {:noreply, state.update(processes: HashDict.delete(state.processes, name))}
  end

  def handle_cast({:add_master_db, host, port, pass, reconnect}, state) do
    subscriber = subscriber([host, port, pass, reconnect])
    subscribe(subscriber)

    {:ok, connection} = :eredis.start_link(host, port, 0, pass, reconnect)

    
    newstate = state.update(main_connection: connection, pubsub_connection: subscriber, connected: true, connections: [{host, port, pass, reconnect} | state.connections])

   #"indexes"})
   #:gen_server.call(:architecture_server, {:create_ring, "bits"})
   #:gen_server.call(:architecture_server, {:create_ring, "counters"})
    
    {:noreply, newstate}
  end





  def handle_cast({:subscribed, from}, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "connected::#{atom_to_binary(node())}"])
    {:noreply, state}
  end

  def handle_cast({:message, message}, state) do
    localnode = atom_to_binary(node())
    newstate = case message do
      <<"connected::", othernode::binary>> ->   
        case :net_adm.ping binary_to_atom(othernode) do
          :pong -> 
            case  othernode == localnode do
              true -> state
              false ->
                    tempstate = state.update(nodes: HashSet.put(state.nodes, othernode))
                    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "welcome::#{atom_to_binary(node())}"])
                    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "send:handhake::from:#{localnode}::to::#{othernode}"])
                    :gen_server.cast(:weissmuller, :sync_dbs)
                    :ok = send_handshake(binary_to_atom("architecture-server-#{othernode}"), localnode, tempstate)
                    tempstate
              end
                    
          _     -> state
        end
      <<"welcome::", othernode::binary>> ->
        case :net_adm.ping binary_to_atom(othernode) do
          :pong ->
            state.update(nodes: HashSet.put(state.nodes, othernode))
          _ -> state
        end
          

      _ -> state
    end
    {:noreply, newstate}
  end

  def handle_cast({:eredis_disconnected, from}, state) do
    {:noreply,  state.update(connected: false)}
  end

  def handle_cast({:eredis_connected, from}, state) do
    {:noreply,  state.update(connected: true)}
  end

  def handle_cast({:handshake, thenode, processes}, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "receive:handshake::from#{thenode}::to:#{atom_to_binary(node())}"])

    {:noreply,  state.update(processes: HashDict.merge(state.processes, processes), nodes: HashSet.put(state.nodes, thenode))}
  end

  def handle_cast({:remove_node, thenode }, state) do
    {:noreply,  state.update(nodes: HashSet.delete(state.nodes, thenode))}
  end

  def handle_cast({:log, message}, state) do
    :eredis.q(state.main_connection, ["PUBLISH", "nodes", "#{node()}::log::#{message}"])
    {:noreply, state}
  end

  def handle_info(:pingnodes, state) do
    ping_the_guys(state.nodes)
    :erlang.send_after(@ping_delay, self(), :pingnodes)

    :eredis.q(state.main_connection, ["ZADD", "aktivat-nodes-lb", "#{Enum.count(Process.list)}", "#{state.hostaddress}"])
    {:noreply, state}
  end

  def terminate(_reason, state) do
    Enum.map(state.nodes, fn(othernode) -> 
      :gen_server.cast(:global.whereis_name(binary_to_atom("architecture-server-#{othernode}")), {:remove_node, atom_to_binary(node())}) end
    )
    :ok
  end

  def subscriber([host, port, pass, reconnect]) do
    {ok, client} = :eredis_sub.start_link(host, port, pass, reconnect, :infinity, :drop)
    client
  end

  def subscribe(subscriber) do
    spawn_link(fn -> 
      :eredis_sub.controlling_process(subscriber)
      :eredis_sub.subscribe(subscriber, ["nodes"])
      receiver(subscriber) 
    end )
  end

  def receiver(subscriber) do
    receive do
      
      {:subscribed, channel, from} ->
        :eredis_sub.ack_message(subscriber)
        :gen_server.cast(:architecture_server, {:subscribed, from})
        receiver(subscriber)

      {:message, channel, message, from} ->
        :eredis_sub.ack_message(subscriber) 
        :gen_server.cast(:architecture_server, {:message, message})
        receiver(subscriber)

      {:eredis_disconnected, from} ->
        :eredis_sub.ack_message(subscriber) 
        :gen_server.cast(:architecture_server, {:eredis_disconnected, from})
        receiver(subscriber)

      {:eredis_connected, from} ->
        :eredis_sub.ack_message(subscriber)
        :gen_server.cast(:architecture_server, {:eredis_connected, from})
        receiver(subscriber) 

      msg -> IO.inspect msg
        :eredis_sub.ack_message(subscriber)
        receiver(subscriber) 
    end
  end


  def ping_the_guys(thenodes) do
    Enum.map(thenodes, fn(thenode) ->
        case :net_adm.ping(binary_to_atom(thenode)) do
          :pong -> :ok
          _ -> :gen_server.cast(:architecture_server, {:remove_node, thenode})
        end
      end)
    :ok
  end

  def send_handshake(remotenode, localnode, state) do
    case :global.whereis_name(remotenode) do
      :undefined -> send_handshake(remotenode, localnode, state)
      address -> :gen_server.cast(address, {:handshake, localnode, state.processes})
                 :ok  
    end
  end


end