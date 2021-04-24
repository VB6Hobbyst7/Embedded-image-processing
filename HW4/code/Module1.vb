Module Module1
    Public mix As Bitmap
    Function gray_scale(bm As Bitmap) As Bitmap '灰階化
        Dim gray As Bitmap = bm.Clone
        For y = 0 To bm.Height - 1
            For x = 0 To bm.Width - 1
                Dim c As Color = bm.GetPixel(x, y)
                Dim sum As Integer = (Val(c.R) + Val(c.G) + Val(c.B)) / 3
                gray.SetPixel(x, y, Color.FromArgb(sum, sum, sum))
            Next
        Next
        Return gray
    End Function

    Public map_lbp(,) As Integer '各像素的LBP值
    Function lbpTransform(bm As Bitmap, ByRef roi() As Integer) As Bitmap
        Dim lbp As Bitmap = gray_scale(bm)
        ReDim map_lbp(lbp.Width, lbp.Height)
        For y = 0 To lbp.Height - 1
            For x = 0 To lbp.Width - 1
                Dim map(2, 2) As String
                Dim c As Color = lbp.GetPixel(x, y)
                Dim m As Integer = c.R
                For i = y - 1 To y + 1
                    For j = x - 1 To x + 1
                        If i >= 0 And i < lbp.Height And j >= 0 And j < lbp.Width Then
                            c = lbp.GetPixel(j, i)
                            Dim g As Integer = c.R
                            If g >=
                                m Then
                                map(j - x + 1, i - y + 1) = 1
                            Else
                                map(j - x + 1, i - y + 1) = 0
                            End If
                        End If
                    Next
                Next
                Dim st As String = (map(2, 1) & map(2, 0) & map(1, 0) & map(0, 0) & map(0, 1) & map(0, 2) & map(1, 2) & map(2, 2))
                Dim num As Integer = Convert.ToInt32(st, 2) '二進制轉十進制
                map_lbp(x, y) = num '存放LBP值
                lbp.SetPixel(x, y, Color.FromArgb(num, num, num)) '將該像素設成LBP值
                roi(num) += 1
            Next
        Next
        Return lbp
    End Function

    Public map_sobel(,) As Integer
    Function sobelTransform(bm As Bitmap, ByRef threshold As Integer) As Bitmap '索伯運算子(邊緣檢測)
        Dim gray As Bitmap = gray_scale(bm)
        Dim sobel As Bitmap = bm.Clone
        Dim map(bm.Width, bm.Height) As Integer
        Dim kernal_x(,) As Integer = {{-1, 0, 1}, '左右檢測
                      {-2, 0, 2},
                      {-1, 0, 1}}
        Dim kernal_y(,) As Integer = {{-1, -2, -1},'上下檢測
                      {0, 0, 0},
                      {1, 2, 1}}
        ReDim map_sobel(bm.Width, bm.Height)
        For y = 0 To bm.Height - 1 '將灰階值存入二維陣列中
            For x = 0 To bm.Width - 1
                Dim c As Color = gray.GetPixel(x, y)
                map(x, y) = Val(c.R) '由於gray已經灰階化
            Next
        Next

        For y = 0 To bm.Height - 1
            For x = 0 To bm.Width - 1
                Dim Gx As Integer = 0
                Dim Gy As Integer = 0
                For i = y - 1 To y + 1
                    For j = x - 1 To x + 1
                        If i >= 0 And j >= 0 And i < bm.Height And j < bm.Width Then
                            Gx += map(j, i) * kernal_x(i - y + 1, j - x + 1) '由於VB定義二維陣列的方向並非(x,y)，所以使用kernal方向相反
                            Gy += map(j, i) * kernal_y(i - y + 1, j - x + 1)
                        End If
                    Next
                Next
                Dim G As Integer = Math.Sqrt(Gx ^ 2 + Gy ^ 2)
                If G > 255 Then G = 255
                map_sobel(x, y) = G
                If G >= threshold Then
                    sobel.SetPixel(x, y, Color.FromArgb(G, G, G))
                Else
                    sobel.SetPixel(x, y, Color.Black)
                End If
            Next
        Next
        Return sobel
    End Function
End Module
