defmodule Data.Weissmuller do
  use GenServer.Behaviour

  defrecord State, indexes: nil, bits: nil, counters: nil, logs: nil, servernames: nil, used_servernames: [], origin_servernames: nil

  @rings [:indexes, :bits, :counters, :logs]

  def add_servers(ring, servers) do
    Enum.each(servers, fn(server) ->  
       :gen_server.call(:weissmuller, {:prepare_server, ring, server})
    end)
  end

  def add_servers(ring, node, servers, used_servernames) do
    Enum.each(servers, fn(server) ->  
      Architecture.Server.propagate_db(ring, node, server, used_servernames)
    end)
  end

  def add_server(ring, server) do
     :gen_server.call(:weissmuller, {:prepare_server, ring, server})
  end

  def add_server(ring, node, server, used_servernames) do
     Architecture.Server.propagate_db(ring, node, server, used_servernames)
  end

  def receive_db(ring, node, server, used_servernames) do
    :gen_server.call(:weissmuller, {:add_server, ring, node, server, used_servernames})
  end

  def status() do
    :gen_server.call(:weissmuller, :status)
  end

  def start_link() do
    :gen_server.start_link({ :local, :weissmuller }, __MODULE__, [], [])
  end

  def init(_) do
    :global.register_name(binary_to_atom("weissmuller-#{atom_to_binary(node())}"), self)     
    servernames = Enum.map(File.stream!("#{:code.priv_dir(:trackr)}/servers.txt"), &String.rstrip(&1))


    {ok, temp_config} = :yaml.load_file("config.yml", [:implicit_atoms])
    config = :erlson.from_nested_proplist( :lists.flatten(temp_config))
    IO.puts "Setting up redis cluster"
    IO.puts "========================"
    Enum.each(@rings, fn(ring) ->   
      :gen_server.cast(:architecture_server, {:create_ring, atom_to_binary(ring)})
      case :erlson.get_value([:redis, ring], config) do
        :undefined -> :ok
        [config] -> 
          IO.puts "Setting up #{atom_to_binary(ring)} ring... "
          :erlang.send_after(100, self(), {:setup_redis_servers, atom_to_binary(ring), config})
      end
    end)
    :erlang.send_after(1000, self(), :check_redis_configs)
    Trackr.launch_sessions
    {:ok, State.new(indexes: HashDict.new(), bits: HashDict.new(), counters: HashDict.new(), logs: HashDict.new(), servernames: servernames, origin_servernames: servernames)}
  end

  

  def handle_call({:add_server, ring, thenode, server, used_servernames}, _from, state) do
    servernames = Enum.filter(state.origin_servernames, fn(x) -> case Enum.member?(used_servernames, x) do
      true -> false
      false -> true end
     end)
    t = Enum.filter(servernames, fn(x) -> if x == thenode do false else true end end)
    :gen_server.cast(:architecture_server, {:add_node, ring, thenode})
    :hottub.start_link(binary_to_atom(thenode), 5, Data.Redis.Server, :start_link, [[{:ring, ring}, {:servername, thenode}]])
    Enum.each(:gen_server.call(binary_to_atom(thenode), {:list_workers}), fn(x) -> 
      :gen_server.cast(x, {:setup, server})
    end)
    server = case HashDict.has_key?(apply(state, binary_to_atom(ring), []), thenode) do
      true  -> HashDict.put(apply(state, binary_to_atom(ring), []), thenode, HashSet.put(HashDict.fetch!(apply(state, binary_to_atom(ring), []), thenode), server))
      _     -> HashDict.put(apply(state, binary_to_atom(ring), []), thenode, HashSet.new([server]))
    end
    {:reply, {:ok, {thenode,server}}, state.update([{binary_to_atom(ring), server}, {:servernames, t}, {:used_servernames, List.flatten(Enum.uniq(used_servernames))}])}
  end

  def handle_call({:prepare_server, ring, server}, _from, state) do
    [thenode|servernames] = state.servernames
    Architecture.Server.propagate_db(ring, thenode, server, state.used_servernames)
    {:reply, {ring, thenode, server}, state.update(servernames: servernames)}
  end

  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end

  def handle_cast(:sync_dbs, state) do
    case state.bits do
      nil -> :ok
      bits   -> Enum.each(bits, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("bits", thenode, server, state.used_servernames) 
        end) 
      end )
    end
    case state.indexes do
      nil -> :ok
      indexes   -> Enum.each(indexes, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("indexes", thenode, server, state.used_servernames) 
        end)
      end )
    end
    case state.counters do
      nil -> :ok
      counters   -> Enum.each(counters, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("counters", thenode, server, state.used_servernames) 
        end)
      end )
    end
    case state.logs do
      nil -> :ok
      logs   -> Enum.each(logs, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("logs", thenode, server, state.used_servernames) 
        end)
      end )
    end
    {:noreply, state}
  end


  def handle_info(:check_redis_configs, state) do
    used_servernames = Dict.keys(Dict.merge(Dict.merge(state.indexes, state.bits), Dict.merge(state.logs, state.counters)))
    used_servernames = List.flatten(Enum.uniq([used_servernames|state.used_servernames]))
    servernames = Enum.filter(state.origin_servernames, fn(x) -> case Enum.member?(used_servernames, x) do
      true -> false
      false -> true end
     end)
    :erlang.send_after(1000, self(), :check_redis_configs)
    {:noreply, state.update(servernames: servernames, used_servernames: List.flatten(Enum.uniq(used_servernames)))}
  end


  def handle_info({:setup_redis_servers, ring, servers}, state) do
    used_servernames = Dict.keys(Dict.merge(Dict.merge(state.indexes, state.bits), Dict.merge(state.logs, state.counters)))
    used_servernames = List.flatten(Enum.uniq([used_servernames|state.used_servernames]))
    servernames = Enum.filter(state.origin_servernames, fn(x) -> case Enum.member?(used_servernames, x) do
      true -> false
      false -> true end
     end)

    [thenode|servernames] = state.servernames
    case servers do
      [] -> :ok
      servers ->
        Enum.each(servers, fn(r) -> 
          {_, args} = r
          pass = case Dict.get(args, :pass) do
            nil -> []
            other -> other
          end
          server = {String.to_char_list!(Dict.get(args, :host)), Dict.get(args, :port), Dict.get(args, :database), pass, Dict.get(args, :reconnect)}


          case Enum.member?(
            List.flatten(
              Enum.map(:erlang.apply(state, binary_to_atom(ring), []), fn(x) -> 
                {_,v} = x 
                Enum.map(v, fn(y) -> y end) end)), server) do
            false -> Architecture.Server.propagate_db(ring, thenode, server, [thenode|used_servernames])
            true -> :ok
          end
          
        end)
    end
    {:noreply, state.update(servernames: servernames, used_servernames: used_servernames)}
  end
  

end
