//****************************************************************************
// ActionScript Standard Library
// FileReference object
//****************************************************************************

intrinsic class flash.net.FileReference
{
	var creationDate:Date;
	var modificationDate:Date;
	var name:String;
	var size:Number;
	var creator:String;
	var type:String;
	var postData:String;
	
	public function FileReference();
	public function browse(typelist:Array):Boolean;
	public function upload(url:String, uploadDataFieldName:String, testUpload:Boolean):Boolean;
	public function download(url:String, defaultFileName:String):Boolean;
	public function cancel():Void;
	function addListener(listener:Object):Void;
	function removeListener(listener:Object):Boolean;	
}