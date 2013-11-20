defmodule Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    Process.register(self, :main_supervisor)
    children = [ 
      
      worker(Architecture.Supervisor, []),
      worker(Data.Supervisor, []),
      worker(Sessions.Supervisor, []) 

    ]
    supervise children, strategy: :one_for_one
  end
end
