class_name Player
extends CharacterBody2D

# class_name CommentedOut
# extends Node2D
# const Fake = preload("res://nope.gd")

const SlimeScript = preload("res://enemies/slime.gd")
const VendorWidget = preload("res://addons/vendor/widget.gd")

var flavor = "extends Node"

func _ready():
	pass

func jump(strength):
	velocity.y = -strength
