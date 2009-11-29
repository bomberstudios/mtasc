//****************************************************************************
// ActionScript Standard Library
// NetStream object
//****************************************************************************

dynamic intrinsic class NetStream
{
	var bufferLength:Number;
	var bufferTime:Number;
	var bytesLoaded:Number;
	var bytesTotal:Number;
	var currentFps:Number;
	var liveDelay:Number;
	var time:Number;
	var checkPolicyFile : Boolean;

	function NetStream(connection:NetConnection);

	function attachAudio(theMicrophone:Microphone):Void;
	function attachVideo(theCamera:Camera,snapshotMilliseconds:Number):Void;
	function close():Void;
	function pause(flag:Boolean):Void;
	function play(name:Object, start:Number, len:Number, reset:Object):Void;
	function publish(name:Object, type:String):Void;
	function receiveAudio(flag:Boolean):Void;
	function receiveVideo(flag:Object):Void;
	function seek(offset:Number):Void;
	function send(handlerName:String):Void;
	function setBufferTime(bufferTime:Number):Void;
	function onResult(streamId:Number);
	function onStatus(infoObject:Object):Void;
	 function onCuePoint(infoObject:Object):Void;
}