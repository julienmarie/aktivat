defmodule Architecture.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    Process.register(self, :architecture_supervisor)
    children = [ 
      worker(Architecture.Server, [],restart: :permanent  )
    ]
    supervise children, strategy: :one_for_one
  end
end
