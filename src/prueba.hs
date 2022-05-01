aplicarDescuento :: Num a => a -> a -> a
aplicarDescuento unPrecio unDescuento = unPrecio * (1 - unDescuento)

aplicarCostoDeEnvio :: Num a => a -> a -> a 
aplicarCostoDeEnvio unPrecio costoDeEnvio = unPrecio + costoDeEnvio

precioTotal ::  Num a => a -> a -> a -> a -> a
precioTotal unaCantidad unPrecio unDescuento costoDeEnvio =
aplicarDescuento unPrecio unDescuento * unaCantidad + costoDeEnvio 

entregaSencilla :: Eq a => a -> Bool 
entregaSencilla = even . show

descodiciarProducto :: String -> Char 
descodiciarProducto nombreDeUnProducto = head nombreDeUnProducto

productoCodiciado :: String -> Bool 
productoCodiciado nombreDeUnProducto = (length nombreDeUnProducto) == 10

productoCorriente :: String -> Bool 
productoCorriente nombreDeUnProducto = head nombreDeUnProducto == 'a' || head nombreDeUnProducto == 'e' || head nombreDeUnProducto == 'i' || head nombreDeUnProducto == 'o' || head nombreDeUnProducto == 'u'   

productoXL :: String -> String 
productoXL nombreDeUnProducto = nombreDeUnProducto ++ "XL"

versionBarata :: String -> String
versionBarata = reverse . descodiciarProducto