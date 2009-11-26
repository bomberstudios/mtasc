//****************************************************************************
// ActionScript Standard Library
// Stage object 
//****************************************************************************

intrinsic class Stage
{
	static var align:String;
    static var displayState:String;
	static var height:Number;
	static var scaleMode:String;
	static var showMenu:Boolean;
	static var width:Number;

	static function addListener(listener:Object):Void;
	static function removeListener(listener:Object):Boolean;

    function onFullScreen(bFull:Boolean):Void;
}