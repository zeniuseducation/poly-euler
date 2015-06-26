defmodule App do
	use Mix.Project
	
	def project do
		[app: :app,
		version: "1.0.0",
		deps: deps]
	end

	defp deps do
    [{:couchie, "~> 0.1"},
     {:plug, github: "nirvana/couchie"}]
  	end
end
