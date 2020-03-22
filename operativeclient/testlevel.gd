extends Node2D

func _ready():
	GlobalVariables.connect("game_object_changed", self, "_on_game_object_changed")

func _on_game_object_changed(object):
	if object["type"] == "laser" and object["name"] == "las0":
		$las0.visible = object["enabled"]
		$las0/Area2D/CollisionShape2D.disabled = !object["enabled"]

func _process(delta):
	var direction = Vector2(0, 0)
	if Input.is_action_pressed("ui_up"):
		direction += Vector2(0, -1)
	if Input.is_action_pressed("ui_down"):
		direction += Vector2(0, 1)
	if Input.is_action_pressed("ui_left"):
		direction += Vector2(-1, 0)
	if Input.is_action_pressed("ui_right"):
		direction += Vector2(1, 0)
	$player.translate(direction.normalized() * 2)

func _on_Area2D_area_entered(area):
	if area.get_parent().is_in_group("hazard"):
		$player.visible = false
