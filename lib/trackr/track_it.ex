defmodule Trackr.TrackIt do

  def init(_transport, req, []) do
    {:ok, req, nil}
  end

  def handle(req, state) do    
    
    send_to_v_user(req)
    
    {:ok, req} = :cowboy_req.reply(200, [{"content-type", "image/gif"}], "", req)
    {:ok, req, state}
  end

  def terminate(_reason, _req, _state), do: :ok

  defp send_to_v_user(req) do

    #Setting up variables
    {qs, _}           = :cowboy_req.qs_vals(req)
    {{ip, _}, _}      = :cowboy_req.peer(req)
    {headers, _}      = :cowboy_req.headers(req)
    v_user            = :proplists.get_value("config_cookies_v_visitor",qs)
    {header_host, _}  = :cowboy_req.host(req)
    account           = :proplists.get_value("config_account",qs)
    trackr_host       = :proplists.get_value("hostname",qs)

    #Linking to server
    case header_host == trackr_host do # Should validate account and host here
      true ->   server_name = "#{trackr_host}__#{account}__#{v_user}"
                IO.puts "Request arriving here :      #{server_name}"
                case Architecture.Server.findp(server_name) do

                  :not_found ->           Sessions.Server.start(Process.whereis(:sessions_supervisor),server_name, v_user, account, trackr_host,  qs,ip,headers)

                                          :ok

                  pid when is_pid(pid) -> :gen_server.cast(pid, {:request,v_user,qs,ip,headers})
                                          :ok

                  _ ->                    :ok

                end
      false ->  :ok

    end
 
  end

end