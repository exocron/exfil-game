extends Sprite

export var health = 5
var direction = Vector2(0, 0)
var dead = false

signal dead

func _process(delta):
	if dead:
		free()
		return
	direction = Vector2(0, 0)
	if Input.is_action_pressed("ui_up"):
		direction += Vector2(0, -1)
	if Input.is_action_pressed("ui_down"):
		direction += Vector2(0, 1)
	if Input.is_action_pressed("ui_left"):
		direction += Vector2(-1, 0)
	if Input.is_action_pressed("ui_right"):
		direction += Vector2(1, 0)
	translate(direction.normalized() * 2)

func _on_Area2D_area_entered(area):
	if area.get_parent().is_in_group("hazard"):
		health -= 1
		if health == 0:
			emit_signal("dead")
			dead = true
		else:
			translate(direction.normalized() * -16)
