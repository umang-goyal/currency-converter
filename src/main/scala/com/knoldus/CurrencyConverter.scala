package com.knoldus

class CurrencyConverter {

  /**
   * Converter class is a case class for all the currency converter. This class gives the template to convert
   * to a currnecy from other currency.
   * @param usd   conversion rate for usd
   * @param euro  conversion rate for euro
   * @param pound conversion rate for pound
   * @param swiss conversion rate for swiss
   * @param yen conversion rate for yen
   * @param rupees conversion rate for rupees
   * @param cad conversion rate for cad
   * @param aud conversion rate for aud
   * @param dinar conversion rate for dinar
   * @param yuan conversion rate for yuan
   */

  case class Converter(usd: Double, euro: Double, pound: Double, swiss: Double, yen: Double,
                       rupees: Double, cad: Double, aud: Double, dinar: Double, yuan: Double) {

    /**
     * rate is a map that stores the conversion rate for a currency from all other currency
     */

    val rate = Map("usd" -> usd, "euro" -> euro, "pound" -> pound, "swiss" -> swiss, "yen" -> yen,
      "rupees" -> rupees, "cad" -> cad, "aud" -> aud,
      "dinar" -> dinar, "yuan" -> yuan)

    /**
     * convert method returns the converted value currency to which it belongs to.
     * @param amount amount to be converted
     * @param currency current currency of the amount
     * @return converted currency of type Double
     */

    def convert(amount: Double, currency: String): Double = {
      amount * rate(currency)
    }
  }

  val Usd: Converter = Converter(1, 1.09, 1.29, 1.02, 0.0091, 0.014, 0.75, 0.67, 3.29, 0.14)
  val Euro: Converter = Converter(0.91, 1, 1.18, 0.93, 0.0083, 0.013, 0.69, 0.61, 3.0, 0.13)
  val Pound: Converter = Converter(0.78, 0.85, 1, 0.79, 0.0071, 0.011, 0.58, 0.52, 2.55, 0.11)
  val Swiss: Converter = Converter(0.98, 1.07, 1.26, 1, 0.0089, 0.014, 0.73, 0.65, 3.22, 0.14)
  val Yen: Converter = Converter(109.73, 120.11, 141.56, 112.19, 1, 1.53,  82.44, 73.24, 360.71, 15.68)
  val Rupees: Converter = Converter(71.5, 78.28, 92.18,73.12, 0.65, 1, 53.73, 57.72, 235.09, 10.21)
  val Cad: Converter = Converter(1.33, 1.46, 1.72, 1.36, 0.012, 0.019, 1, 0.89, 4.37, 0.19)
  val Aud: Converter = Converter(1.50, 1.64, 1.93, 1.53, 0.014, 0.021, 1.13, 1, 4.93, 0.21)
  val Dinar: Converter = Converter(0.30, 0.33, 0.39, 0.31, 0.0028, 0.0043, 0.23, 0.30, 1, 0.043)
  val Yuan: Converter = Converter(7.0,  7.66,  9.02,  7.16, 0.064, 0.098, 5.26, 4.67, 23.02, 1)

  /**
   * checks if converter for the currency is available or not
   * @param current current currency
   * @param future future currency
   * @return returns true if converter id available else false
   */

  def exceptionCheck(current: String, future: String): Boolean ={
    val acceptedCurrency = List("usd", "euro", "pound", "swiss", "yen",
      "rupees", "cad", "aud" ,
      "dinar", "yuan")
    if (acceptedCurrency.contains(current) && acceptedCurrency.contains(future)){
      true
    }
    else{
      false
    }
  }

  /**
   * converterSelector calls the required currency converter class for the given values.
   * @param amount numeric value of the amount to be converted
   * @param currentCurrency current currency of the amount
   * @param futureCurrency currency to which the current is to be converted
   * @return converted amount in the future currency, type double
   */

  def converterSelector(amount: Double, currentCurrency: String, futureCurrency: String): Double = {
    if (amount<0){
      throw new Exception ("amount cannot be negative")
    }
      else if(exceptionCheck(currentCurrency.toLowerCase(), futureCurrency.toLowerCase())){

      futureCurrency.toLowerCase() match {
        case "usd" => Usd.convert(amount, currentCurrency.toLowerCase())
        case "euro" => Euro.convert(amount, currentCurrency.toLowerCase())
        case "pound" => Pound.convert(amount, currentCurrency.toLowerCase())
        case "swiss" => Swiss.convert(amount, currentCurrency.toLowerCase())
        case "yen" => Yen.convert(amount, currentCurrency.toLowerCase())
        case _ => converterSelectorExtended(amount, currentCurrency.toLowerCase(), futureCurrency.toLowerCase())
      }
    }

    else {
      throw new Exception ("conversion for this not available")
    }
  }

  private def converterSelectorExtended(amount: Double, currentCurrency: String, futureCurrency: String):Double={
    futureCurrency match {
      case "rupees" => Rupees.convert(amount, currentCurrency.toLowerCase())
      case "cad" => Cad.convert(amount, currentCurrency.toLowerCase())
      case "aud" => Aud.convert(amount, currentCurrency.toLowerCase())
      case "dinar" => Dinar.convert(amount, currentCurrency.toLowerCase())
      case "yuan" => Yuan.convert(amount, currentCurrency.toLowerCase())
    }
  }
}