defmodule Sessions.Server do
  use GenServer.Behaviour

  defrecord State, session_id: nil, ip: nil, virtual_user_id: nil, user_id: nil, qs: nil, headers: nil, current_pages: nil

  #current_pages format : [{TabId, Url, Elapsed Time, InitTimestamp, ActualTimestamp}]

  def start(sup, server_name, session, qs, ip, headers) do
    :supervisor.start_child(sup, [server_name, session, qs, ip, headers])
  end


  def start_link(server_name, session,qs,ip,headers) do

    :gen_server.start_link( __MODULE__, [server_name,session,qs,ip,headers], [])
    
  end


  def init([server_name,session,qs,ip,headers]) do
    :ok = Architecture.Server.reg(server_name, self())
    process_request(session, qs, ip, headers)
    {:ok, State.new(session_id: session, ip: ip, headers: headers, qs: qs)}
  end

  def handle_call(request, from, state) do
    super(request, from, state)
  end

  def handle_cast({:request,session,qs,ip,headers},from, state) do
    process_request(session, qs, ip, headers)
    {:reply, :ok, state}
  end

  def process_request(_session, _qs, _ip, _headers) do
    :ok
  end

end