defmodule Data.Redis.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    Process.register(self, :redis_supervisor)
    #{ok, temp_config }= :yaml.load_file("config.yml", [:implicit_atoms])
    #config = :erlson.from_nested_proplist( :lists.flatten(temp_config))
#
    #pools = :erlson.get_value([:redis], config)

    #poolsetup = fn({name, [size_args, worker_args]}) ->
    #  pool_args = [{:name, {:local, name}}, {:worker_module, Data.Redis.Server}] ++ size_args
    #  :poolboy.child_spec(name, pool_args, worker_args)
    #end
#
    #poolspecs = :lists.map poolsetup, pools
    children = []

    supervise children, strategy: :one_for_one
  end

end
