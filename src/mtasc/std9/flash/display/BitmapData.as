//****************************************************************************
// ActionScript Standard Library
// flash.display.BitmapData object
//****************************************************************************
import flash.geom.Matrix;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.geom.ColorTransform;
import flash.filters.BitmapFilter;

intrinsic class flash.display.BitmapData
{
	var width:Number;
	var height:Number;
	var transparent:Boolean;
	var rectangle:Rectangle;

	static function loadBitmap(id:String):BitmapData;

	function BitmapData (width:Number, height:Number, transparent:Boolean, fillColor:Number);
	function clone():BitmapData;
	function getPixel(x:Number, y:Number):Number;
	function getPixel32(x:Number, y:Number):Number;
	function setPixel(x:Number, y:Number, color:Number):Void;
	function setPixel32(x:Number, y:Number, color:Number):Void;
	function applyFilter (sourceBitmap:BitmapData, sourceRect:Rectangle, destPoint:Point, filter:BitmapFilter):Number;
	function colorTransform(rect:Rectangle, colorTransform:ColorTransform):Void;
	public function copyChannel(sourceBitmap:BitmapData, sourceRect:Rectangle,
		destPoint:Point, sourceChannel:Number, destChannel:Number):Void;
	public function copyPixels(sourceBitmap:BitmapData,
									  sourceRect:Rectangle,
									  destPoint:Point,
									  alphaBitmap:BitmapData,
									  alphaPoint:Point,
									  mergeAlpha:Boolean):Void;
	public function dispose():Void;
	public function draw(source:Object,
							matrix:Matrix,
							colorTransform:ColorTransform,
							blendMode:Object,
							clipRect:Rectangle,
							smooth:Boolean):Void;
	public function fillRect (rect:Rectangle, color:Number):Void;
	public function floodFill (x:Number, y:Number, color:Number):Void;
	public function generateFilterRect (sourceRect:Rectangle, filter:BitmapFilter):Rectangle;
	public function getColorBoundsRect (mask:Number, color:Number, findColor:Boolean):Rectangle;
	public function hitTest(firstPoint:Point,
							   firstAlphaThreshold:Number,
							   secondObject:Object,
							   secondBitmapPoint:Point,
							   secondAlphaThreshold:Number):Boolean;
	public function merge(sourceBitmap:BitmapData,
								 sourceRect:Rectangle,
								 destPoint:Point,
								 redMult:Number,
								 greenMult:Number,
								 blueMult:Number,
								 alphaMult:Number):Void;
	public function noise(randomSeed:Number, low:Number, high:Number,
								 channelOptions:Number,
								 grayScale:Boolean):Void;
	public function paletteMap(sourceBitmap:BitmapData,
								  sourceRect:Rectangle,
								  destPoint:Point,
								  redArray:Array,
								  greenArray:Array,
								  blueArray:Array,
								  alphaArray:Array):Void;
	public function perlinNoise(baseX:Number, baseY:Number,
									   numOctaves:Number, randomSeed:Number,
									   stitch:Boolean, fractalNoise:Boolean,
									   channelOptions:Number,
									   grayScale:Boolean,
									   offsets:Object):Void;
	public function pixelDissolve(sourceBitmap:BitmapData,
										 sourceRect:Rectangle,
										 destPoint:Point,
										 randomSeed:Number,
										 numberOfPixels:Number,
										 fillColor:Number):Number;
	public function scroll(x:Number, y:Number):Void;
	public function threshold(sourceBitmap:BitmapData,
								 sourceRect:Rectangle,
								 destPoint:Point,
								 operation:String,
								 threshold:Number,
								 color:Number,
								 mask:Number,
								 copySource:Boolean):Number;
	public function compare(otherBitmapData:BitmapData):Object;								 
}