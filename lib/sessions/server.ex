defmodule Sessions.Server do
  use GenServer.Behaviour

  defrecord State, server_name: nil, session_id: nil, ip: nil, qs: nil, headers: nil, virtual_user_id: nil, user_id: nil, current_pages: nil, index_pool: nil, bit_pool: nil, counter_pool: nil, log_pool: nil

  @sessiontime 1200000
  #current_pages format : [{TabId, Url, Elapsed Time, InitTimestamp, ActualTimestamp}]

  def start(sup, server_name, session, account, trackr_host, qs, ip, headers) do
    :supervisor.start_child(sup, [server_name, session, account, trackr_host, qs, ip, headers])
  end


  def start_link(server_name, session,account, trackr_host,qs,ip,headers) do
    :gen_server.start_link( __MODULE__, [server_name,session,account, trackr_host,qs,ip,headers], [])
  end


  def init([server_name,session,account,trackr_host,qs,ip,headers]) do
    :ok = Architecture.Server.reg(server_name, self())
    log_pool = case :gen_server.call(:architecture_server, {:find_node, "logs", server_name}) do
      { :error, _ } -> nil
      { :ok, node } -> node
    end
    index_pool = case :gen_server.call(:architecture_server, {:find_node, "indexes", server_name}) do
      { :error, _ } -> nil
      { :ok, node } -> node
    end
    counter_pool = case :gen_server.call(:architecture_server, {:find_node, "counters", server_name}) do
      { :error, _ } -> nil
      { :ok, node } -> node
    end
    bit_pool = case :gen_server.call(:architecture_server, {:find_node, "bits", server_name}) do
      { :error, _ } -> nil
      { :ok, node } -> node
    end

    virtual_user_id = case index_pool do
      nil -> nil
      server -> case :hottub.call(binary_to_atom(server), {:find_id, account, trackr_host, "virtual_user_id", session}) do
        {:ok, id} -> id
        _ -> nil
      end
    end


    # NEED TO FIND USER_ID ( Data.Redis.Server.find_id )
    # NEED TO FIND PROPERTY FROM trackr_host ( wildcards, etc... )
    # NEED TO FIND PROPERTY_ID ( Data.Redis.Server.find_id )
    # NEED TO FIND ACCOUNT_ID ( Data.Redis.Server.find_id )

    
    {:ok, State.new(virtual_user_id: "virtual_user_id", server_name: server_name, session_id: session, ip: ip, headers: headers, qs: qs, index_pool: index_pool, bit_pool: bit_pool, log_pool: log_pool, counter_pool: counter_pool), @sessiontime}
  end

  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end

  def handle_cast({:request,session,qs,ip,headers}, state) do
    process_request(session, qs, ip, headers)
    IO.puts "Request being handled here : #{state.server_name}"
    {:noreply, state.update(ip: ip, headers: headers, qs: qs), @sessiontime}
  end


  def handle_info(:timeout, state) do
    :gen_server.cast(:architecture_server, {:remove_process, state.server_name})
    Architecture.Server.log("Quit")
    {:stop, :normal, state}
  end


  def process_request(session, qs, ip, headers) do
    event_type = :proplists.get_value("trackr_event_type",qs)
    IO.puts "Event type : #{event_type}"
      case is_function(binary_to_atom(event_type)) do
        true -> apply(binary_to_atom(event_type), [session, qs, ip, headers])
        false -> :ok
    end
  end

end