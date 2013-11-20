defmodule Data.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    Process.register(self, :data_supervisor)
    children = [ 
      worker(Data.Weissmuller, []) 
    ]
    supervise children, strategy: :one_for_one
  end
end
