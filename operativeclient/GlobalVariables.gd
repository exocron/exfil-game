extends Node

var playerName: String = ""
var _ws: WebSocketClient = null
var _token: String = ""
var _code: String = ""

var code: String setget _code_set, _code_get

func _ready():
	ws_connect()

func ws_connect():
	if _ws:
		_ws.disconnect_from_host()
	_ws = WebSocketClient.new()
	_ws.connect("connection_closed", self, "_on_ws_closed")
	_ws.connect("connection_error", self, "_on_ws_closed")
	_ws.connect("connection_established", self, "_on_ws_established")
	_ws.connect("data_received", self, "_on_ws_data_received")
	if _code:
		_ws.connect_to_url("ws://127.0.0.1:8080/join")
	else:
		_ws.connect_to_url("ws://127.0.0.1:8080/new")

func _on_ws_closed(was_clean = false):
	print("Closed")

func _on_ws_established(proto = ""):
	_ws.get_peer(1).set_write_mode(WebSocketPeer.WRITE_MODE_TEXT)
	if _code:
		var data = {"code": _code}
		if _token:
			data["token"] = _token
		data = JSON.print(data)
		_ws.get_peer(1).put_packet(data.to_utf8())
	print("Established")

func _on_ws_data_received():
	var data = _ws.get_peer(1).get_packet().get_string_from_utf8()
	data = JSON.parse(data).get_result()
	if data.has("code"):
		self.code = data["code"]
	if data.has("token"):
		_token = data["token"]
	print("Received data: ", str(data))

func _process(delta):
	_ws.poll()

func _code_get():
	return _code

func _code_set(newcode):
	_code = newcode
	_token = ""
