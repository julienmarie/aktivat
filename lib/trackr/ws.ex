defmodule Trackr.WS do

  def init(_transport, req, _opts, _active) do
    IO.puts "bullet init"
    {cookie, _}      = :cowboy_req.cookie("trackit_v_visitor", req)
    ws_server_name = "ws__#{cookie}"
    IO.puts ws_server_name
    :ok = Architecture.Server.freg(ws_server_name, self())
    tref = :erlang.send_after(1000, self(), :refresh)
    {:ok, req, tref}
  end

  def stream(<<"ping: ", _name :: binary>>, req, state ) do
    {:reply, "pong", req, state}
  end

  def stream(_data, req, state) do
    {:ok, req, state}
  end

  def info(:refresh, req, _) do
    tref = :erlang.send_after(1000, self(), :refresh)
    datetime = :cowboy_clock.rfc1123()
    {:reply, datetime, req, tref}
  end

  def info(info, req, state) do
    {:reply, info, req, state}
  end

  def terminate(_req, tref) do
    :erlang.cancel_timer(tref)
    :ok
  end
end