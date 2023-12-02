Function CalculateBondPrice(faceValue As Double, couponRate As Double, periodsToMaturity As Integer, yieldToMaturity As Double, compoundingFrequency As Integer) As Double
    Dim i As Integer
    Dim couponPayment As Double
    Dim discountedCashFlow As Double
    
    ' Calculate semi-annual coupon payment
    couponPayment = (couponRate * faceValue) / compoundingFrequency
    
    ' Calculate discounted cash flows
    For i = 1 To periodsToMaturity * compoundingFrequency
        discountedCashFlow = discountedCashFlow + (couponPayment / (1 + yieldToMaturity / compoundingFrequency) ^ i)
    Next i
    
    ' Add the face value discounted at the yield to maturity
    discountedCashFlow = discountedCashFlow + (faceValue / (1 + yieldToMaturity / compoundingFrequency) ^ (periodsToMaturity * compoundingFrequency))
    
    ' Return the calculated bond price
    CalculateBondPrice = discountedCashFlow
End Function
