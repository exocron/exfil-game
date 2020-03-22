extends Node2D

func _ready():
	GlobalVariables.connect("game_object_changed", self, "_on_game_object_changed")

func _on_game_object_changed(object):
	if object["type"] == "laser" and object["name"] == "las0":
		$las0.visible = object["enabled"]
		$las0/Area2D/CollisionShape2D.disabled = !object["enabled"]
