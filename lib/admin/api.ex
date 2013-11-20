defmodule Admin.Api do
  def init(_transport, _req, _options) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def allowed_methods(req, state) do
    {["GET", "POST"], req, state}
  end

  def content_types_accepted(req, state) do
    {
      [
        {{"application", "json", []}, :put_json}
      ],
      req, state
    }
  end

  def content_types_provided(req, state) do
    {
      [
        {{"application", "json", []}, :get_json}
      ],
      req, state
    }
  end

  def get_json(req, state) do
    {:ok, _body, _req1} = :cowboy_req.body req
    {:ok, req} = :cowboy_req.reply(200, [], [], req)
    {:ok, req, state}
  end

  def put_json(req, state) do
    :cowboy_req.reply(200, [], [], req)
    {:ok, req, state}
  end


  def terminate(_reason, _req, _state), do: :ok

end