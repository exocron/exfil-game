extends Node

var playerName: String
var _ws: WebSocketClient = null

func start_network():
	_ws = WebSocketClient.new()
	_ws.connect("connection_closed", self, "_on_ws_closed")
	_ws.connect("connection_error", self, "_on_ws_closed")
	_ws.connect("connection_established", self, "_on_ws_established")
	_ws.connect("data_received", self, "_on_ws_data_received")
	_ws.connect_to_url("ws://localhost:8080/new")

func _on_ws_closed(was_clean = false):
	pass

func _on_ws_established(proto = ""):
	pass

func _on_ws_data_received():
	pass

func _process(delta):
	if _ws != null:
		var status = _ws.get_connection_status()
		if status == WebSocketClient.CONNECTION_CONNECTING or status == WebSocketClient.CONNECTION_CONNECTING:
			_ws.poll()
		else:
			_ws.connect_to_url("ws://localhost:8080/new")
