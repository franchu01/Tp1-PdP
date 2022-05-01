-- TP 1

aplicarDescuento :: Num a => (String, a) -> a -> a
aplicarDescuento (_,unPrecio) unDescuento = unPrecio unDescuento

aplicarCostoDeEnvio :: Num a => (String,a) -> a -> a 
aplicarCostoDeEnvio (_,unPrecio) costoDeEnvio = unPrecio + costoDeEnvio

precioTotal ::  Num a => a -> (String,a) -> a -> a -> a
precioTotal unaCantidad (_,unPrecio) unDescuento costoDeEnvio =
    (aplicarDescuento unPrecio unDescuento) * unaCantidad + costoDeEnvio 

entregaSencilla :: String -> Bool 
entregaSencilla = even . length

descodiciarProducto :: Num a => (String,a) -> String
descodiciarProducto (nombreDeUnProducto,_) = take 10 nombreDeUnProducto

productoCodiciado :: Num a => (String,a) -> Bool 
productoCodiciado (nombreDeUnProducto,_) = (length nombreDeUnProducto) > 10

productoCorriente :: Num a => (String,a) -> Bool 
productoCorriente (nombreDeUnProducto,_) = head nombreDeUnProducto == 'a' || head nombreDeUnProducto == 'e' || head nombreDeUnProducto == 'i' || head nombreDeUnProducto == 'o' || head nombreDeUnProducto == 'u'   

productoXL :: Num a => (String,a) -> String 
productoXL (nombreDeUnProducto,_) = nombreDeUnProducto ++ "XL"

versionBarata :: Num a => (String,a) -> String
versionBarata (nombreDeUnProducto,_) = reverse (descodiciarProducto nombreDeUnProducto)


