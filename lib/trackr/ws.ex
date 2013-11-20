defmodule Trackr.WS do

  def init(_transport, req, _opts, _active) do
    IO.puts "bullet init"
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

  def info(_info, req, state) do
    {:ok, req, state}
  end

  def terminate(_req, tref) do
    :erlang.cancel_timer(tref)
    :ok
  end
end