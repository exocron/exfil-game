extends Control

func _on_LineEdit_text_entered(new_text):
	return _on_Button_pressed()

func _on_Button_pressed():
	GlobalVariables.playerName = $LineEdit.text
	get_tree().change_scene("res://net_menu.tscn")
