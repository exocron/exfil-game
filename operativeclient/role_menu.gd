extends Control

func _ready():
	GlobalVariables.connect("peer_role_changed", self, "_on_peer_role_changed")
	$OptionButton.add_item("Select Role", 0)
	$OptionButton.add_item("Operative", 1)
	$OptionButton.add_item("Hacker", 2)
	if GlobalVariables.playerName:
		$YourName.text = GlobalVariables.playerName + "'s role:"
	if GlobalVariables.remoteName:
		$TheirNameAndRole.text = GlobalVariables.remoteName + "'s role: unset"

func _on_OptionButton_item_selected(id):
	var roles = ["undecided", "operative", "hacker"]
	GlobalVariables.role = roles[id]

func _on_peer_role_changed(peer_role):
	if peer_role == "undecided":
		peer_role = "unset"
	if GlobalVariables.remoteName:
		$TheirNameAndRole.text = GlobalVariables.remoteName + "'s role: %s" % [peer_role]
