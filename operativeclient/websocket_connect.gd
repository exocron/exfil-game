extends Node

var _ws = WebSocketClient.new()

func _ready():
	_ws.connect("connection_closed", self, "_on_ws_closed")
	_ws.connect("connection_error", self, "_on_ws_closed")
	_ws.connect("connection_established", self, "_on_ws_established")
	_ws.connect("data_received", self, "_on_ws_data_received")
	print("Connect to URL: " + str(_ws.connect_to_url("ws://127.0.0.1:8080/new")))

func _on_ws_closed(was_clean = false):
	print("Closed")

func _on_ws_established(proto = ""):
	print("Established")

func _on_ws_data_received():
	print("Received")

func _process(delta):
	_ws.poll()
