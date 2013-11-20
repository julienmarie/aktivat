defmodule Sessions.Supervisor do
  use Supervisor.Behaviour
 
  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end
 
  #def start_child(spawner, id, args) do
  #  :supervisor.start_child(spawner, [id|args])
  #end
 #
  #def terminate_child(spawner, id) do
  #  :supervisor.terminate_child(spawner, id)
  #end
  
  def init([]) do
    Process.register(self, :sessions_supervisor)
    children = [ worker(Sessions.Server, [], restart: :temporary) ]
    supervise(children, strategy: :simple_one_for_one)
  end
end