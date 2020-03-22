extends Node

var _playerName: String = ""
var _remoteName: String = ""
var _ws: WebSocketClient = null
var _token: String = ""
var _code: String = ""
var _state = "pregame"
var _role: String = "undecided"

var code: String setget _code_set, _code_get
var playerName: String setget _playerName_set, _playerName_get
var remoteName: String setget ,_remoteName_get
var role: String setget _role_set, _role_get

signal peer_role_changed(peer_role)
signal peer_ready(ready)
signal game_object_changed(object)

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
		if _playerName:
			self.playerName = _playerName
	if data.has("token"):
		_token = data["token"]
	print("Received data: ", str(data))
	if data.has("action"):
		_dispatchActions(data["action"], data)

func _dispatchActions(action, data):
	if action == "rnamechange":
		_remoteName = data["name"]
	if action == "rrolechange" and _state == "role_select":
		emit_signal("peer_role_changed", data["role"])
	if action == "rready" and _state == "role_select":
		emit_signal("peer_ready", data["ready"])
	if action == "statechange" and data["newstate"] == "role_select":
		_state = "role_select"
		get_tree().change_scene("res://role_menu.tscn")
	if action == "statechange" and data["newstate"] == "game_start":
		if _role == "hacker":
			var pid = OS.execute(".\\rsh.exe", [_code, _token], false)
			if pid != -1:
				get_tree().quit()
		else:
			_state = "game_start"
			get_tree().change_scene("res://testlevel.tscn")
	if action == "objectchange":
		emit_signal("game_object_changed", data["object"])

func _process(delta):
	_ws.poll()

func _code_get():
	return "%s-%s-%s" % [_code.substr(0, 3), _code.substr(3, 3), _code.substr(6, 3)]

func _code_set(newcode):
	_code = newcode.replace("-", "").to_upper()
	_token = ""

func _playerName_get():
	return _playerName

func _playerName_set(newname):
	_playerName = newname
	var data = {"action": "setname", "name": newname}
	data = JSON.print(data)
	_ws.get_peer(1).put_packet(data.to_utf8())

func _remoteName_get():
	return _remoteName

func _role_get():
	return _role

func _role_set(newrole):
	if _state == "role_select" and newrole != _role:
		if newrole == "undecided" or newrole == "operative" or newrole == "hacker":
			_role = newrole
			var data = {"action": "setrole", "role": newrole}
			data = JSON.print(data)
			_ws.get_peer(1).put_packet(data.to_utf8())

func signal_ready():
	var data = {"action": "ready", "ready": true}
	data = JSON.print(data)
	_ws.get_peer(1).put_packet(data.to_utf8())
