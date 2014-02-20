defmodule Trackr do
  use Application.Behaviour

  def start(_type, _args) do
    Process.register(self, :trackr_start)
    :application.start :yamler
    :application.start :genx
    :application.start :exreloader
    :application.start :raven


    #Configuration

   
    Supervisor.start_link
    
  end

  def launch_sessions do
     {:ok, temp_config }= :yaml.load_file("config.yml", [:implicit_atoms])
    config = :erlson.from_nested_proplist( :lists.flatten(temp_config))

    :application.set_env(:raven, :error_logger, :erlson.get_value([:sentry_reporting,:error_logger], config) )
    :application.set_env(:raven, :included_applications, :erlson.get_value([:sentry_reporting,:included_applications], config))
    :application.set_env(:raven, :public_key, :erlson.get_value([:sentry_reporting,:public_key], config))
    :application.set_env(:raven, :private_key, :erlson.get_value([:sentry_reporting,:private_key], config))
    :application.set_env(:raven, :project, :erlson.get_value([:sentry_reporting,:project], config))
    :application.set_env(:raven, :uri, :erlson.get_value([:sentry_reporting,:uri], config)) 




    dispatch = :cowboy_router.compile([
      {:_, 
        [
          
          # Controllers
          {:erlson.get_value([:general,:urls,:api_url], config),        Admin.Api,        []},
          {:erlson.get_value([:general,:urls,:tracker_url],config),     Trackr.TrackIt,   []},
          {:erlson.get_value([:general,:urls,:trackr_ws],config),       :bullet_handler,  [{:handler, Trackr.WS}]},
          {"/",           Admin.Home,     []},

          # Static Handling
          {"/css/[...]",  :cowboy_static, [
            {:directory, {:priv_dir, :trackr, [<<"public_prod/css">>]}},
            {:mimetypes, [{<<".css">>, [<<"text/css">>]}]}
          ]},
          {"/js/[...]", :cowboy_static, 
            [
              {:directory, {:priv_dir, :trackr, [<<"public_prod/js">>]}},
              {:mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
            ]
          },
          {"/fonts/[...]", :cowboy_static, 
            [
              {:directory, {:priv_dir, :trackr, [<<"public_prod/fonts">>]}},
              {:mimetypes, [{<<".css">>, [<<"text/css">>]}]}
            ]
          },
          {"/[...]", :cowboy_static, 
            [
              {:directory, {:priv_dir, :trackr, [<<"public_prod/html">>]}},
              {:mimetypes, [{<<".html">>, [<<"text/html">>]}]}
            ]
          }
          

        ]
      }
    ])
    {:ok, [[port]]} = :init.get_argument(:trackrport)
    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: list_to_integer(port)],
                                  [env: [dispatch: dispatch]])

    IO.puts "Handling incoming connection on port #{port}"

    Sessions.Supervisor.start_link
    IO.puts "Aktivat is ready"

  end

  def stop(_state) do
    :ok
  end

end
