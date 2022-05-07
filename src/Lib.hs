-- TP 1

aplicarDescuento :: Num a => (String, a) -> a -> a
aplicarDescuento (_,unPrecio) unDescuento = unPrecio - (unPrecio * unDescuento)

aplicarCostoDeEnvio :: Num a => (String,a) -> a -> a 
aplicarCostoDeEnvio (_,unPrecio) costoDeEnvio = unPrecio + costoDeEnvio

precioTotal ::  Num a => a -> (String,a) -> a -> a -> a
precioTotal unaCantidad unProducto unDescuento costoDeEnvio =
    (aplicarDescuento unProducto unDescuento) * unaCantidad + costoDeEnvio 

entregaSencilla :: String -> Bool 
entregaSencilla = even . length

descodiciarProducto :: Num a => (String,a) -> String
descodiciarProducto (nombreDeUnProducto,_) = take 10 nombreDeUnProducto

productoCodiciado :: Num a => (String,a) -> Bool 
productoCodiciado unProducto = (length unProducto) > 10

esVocal :: Char -> Bool
esVocal unaLetra = unaLetra == 'a' || unaLetra == 'e' || unaLetra == 'i' || unaLetra == 'o' || unaLetra == 'u'

productoCorriente :: Num a => (String,a) -> Bool 
productoCorriente (nombreDeUnProducto,_) = (esVocal . head) nombreDeUnProducto

productoXL :: Num a => (String,a) -> String 
productoXL (nombreDeUnProducto,_) = nombreDeUnProducto ++ "XL"

versionBarata :: Num a => (String,a) -> String
versionBarata unProducto = reverse (descodiciarProducto unProducto)

productoDeLujo :: Num a => (String,a) -> Bool
productoDeLujo (nombreDeUnProducto,_) = elem 'x' nombreDeUnProducto || elem 'z' nombreDeUnProducto

productoDeElite :: Num a => (String,a) -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not . productoCorriente) unProducto


