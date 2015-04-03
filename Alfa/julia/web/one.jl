using Morsel
using Mustache

app = Morsel.app()

tmpl = open (readall,"one.html")

route(app, GET | POST | PUT, "/<user>") do req, res
    nama = convert (ASCIIString, req.state [:route_params] ["user"])
    render (tmpl,{"nama" => nama})
end

get(app, "/about") do req, res
    "This app is running on Morsel"
end

get (app,"/mas") do req,res
    "This is an app"
end

