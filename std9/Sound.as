//****************************************************************************
// ActionScript Standard Library
// Sound object
//****************************************************************************

intrinsic class Sound
{
	var duration:Number;
	var id3:Object;
	var ID3:Object;
	var position:Number;
	var checkPolicyFile : Boolean;

	function Sound(target:Object);

	function onID3():Void;
	function onLoad(success:Boolean):Void;
	function onSoundComplete():Void;	
	function attachSound(id:String):Void;
	function getBytesLoaded():Number;
	function getBytesTotal():Number;
	function getDuration():Number;
	function getPan():Number;
	function getPosition():Number;
	function getTransform():Object;
	function getVolume():Number;
	function loadSound(url:String, isStreaming:Boolean):Void;
	function setDuration(value:Number):Void;
	function setPan(value:Number):Void;
	function setPosition(value:Number):Void;
	function setTransform(transformObject:Object):Void;
	function setVolume(value:Number):Void;
	function start(secondOffset:Number, loops:Number):Void;
	function stop(linkageID:String):Void;
}