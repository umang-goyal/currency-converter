package com.knoldus


import org.scalatest.{BeforeAndAfterAll, FlatSpec}

class CurrencyConverterSpec extends FlatSpec with BeforeAndAfterAll {
  var currencyConverter: CurrencyConverter = new CurrencyConverter

  override def beforeAll(): Unit = {
    currencyConverter = new CurrencyConverter
  }

  override def afterAll(): Unit = {
    if (currencyConverter != null) {
      currencyConverter = null
    }
  }

  "Converter method" should "return amount if current and future currency are same" in {
      val amount = 75
      val actualResult = currencyConverter.converterSelector(amount, "rupees", "RUPEES")
      val expectedResult = 75
      assert(expectedResult == actualResult)
    }


  "Converter method" should "return equivalent amount if converted to Dinar" in {
    val amount= 21
    val actualResult = currencyConverter.converterSelector(amount, "usd", "dinar")
    val expectedResult = 6.3
    assert(expectedResult == actualResult)
  }

  "Converter method" should "return converted amount if converted to AUD" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "yen", "Aud")
    val expectedResult = 0.63
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to USD" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "yen", "usd")
    val expectedResult = 0.40950000000000003
    assert(expectedResult == actualResult)
  }

  "Converter method" should "return converted amount if converted to EURO" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "usd", "euro")
    val expectedResult = 40.95
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to POUND" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "usd", "pound")
    val expectedResult = 35.1
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to SWISS" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "usd", "swiss")
    val expectedResult = 44.1
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to yen" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "usd", "yen")
    val expectedResult = 4937.85
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to CAD" in {
    val amount= 45
    val actualResult = currencyConverter.converterSelector(amount, "usd", "cad")
    val expectedResult = 59.85
    assert(expectedResult == actualResult)

  }

  "Converter method" should "return converted amount if converted to YUAN" in {
    val amount= 45

    val actualResult = currencyConverter.converterSelector(amount, "usd", "yuan")
    val expectedResult = 315.0
    assert(expectedResult == actualResult)
  }

  "Converter method" should "return error of negative number" in {
    val amount = -45
    try {
      val actualResult = currencyConverter.converterSelector(amount, "usd", "yuan")
    }
    catch {
      case e: Exception => assert(e.getMessage == "amount cannot be negative")
    }
  }

  "Converter method" should "return error of currency converter not available" in {
    val amount = 45
    try {
      val actualResult = currencyConverter.converterSelector(amount, "usdd", "yuan")
    }
    catch {
      case e: Exception => assert(e.getMessage == "conversion for this not available")
    }
  }
}
