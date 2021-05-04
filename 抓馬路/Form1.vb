Public Class Form1
    Const W As Integer = 30 'block的寬
    Const L As Integer = 20 'block的長

    Const sample As Integer = 8 '樣本數
    Const fault As Integer = 3 '容錯值
    Private Sub 存檔ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 存檔ToolStripMenuItem.Click
        Dim name As String = InputBox("檔名 : ", "儲存圖片", "img")
        PictureBox1.Image.Save(name & ".bmp")
    End Sub
    Private Sub 開啟圖檔ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 開啟圖檔ToolStripMenuItem.Click
        If OpenFileDialog1.ShowDialog = DialogResult.OK Then
            PictureBox1.Image = Image.FromFile(OpenFileDialog1.FileName)
        End If
    End Sub


    Private Sub 抓取ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 抓取ToolStripMenuItem.Click
        Dim original As Bitmap = PictureBox1.Image
        '測量己耗用的時間
        Dim Watch1 As New Stopwatch
        Watch1.Start()
        sobelTransform(PictureBox1.Image.Clone, 0)
        Watch1.Stop()
        Dim i As Double = Watch1.ElapsedMilliseconds / 1000
        Watch1.Reset()

        '測量己耗用的時間
        Dim Watch2 As New Stopwatch
        Watch2.Start()
        Dim img_lbp As Bitmap = lbpTransform(PictureBox1.Image.Clone)
        Watch2.Stop()
        Dim j As Double = Watch2.ElapsedMilliseconds / 1000
        Watch2.Reset()


        PictureBox1.Image = img_lbp : MsgBox("LBP")



        Dim img_marker As Bitmap = auto_marker(img_lbp)

        '測量己耗用的時間
        Dim Watch3 As New Stopwatch
        Watch3.Start()
        Dim img_watershed As Bitmap = watershed(img_marker, 8)
        Watch3.Stop()
        Dim k As Double = Watch3.ElapsedMilliseconds / 1000
        Watch3.Reset()




        Dim img_road As Bitmap = find_road(original, img_watershed)
        PictureBox1.Image = img_road : MsgBox("馬路抓取")
        MsgBox("sobel : " & i & vbNewLine & "LBP : " & j & vbNewLine & "watershed : " & k,, "執行時間")
    End Sub
    Function find_road(ByVal original As Bitmap, ByVal img_watershed As Bitmap) As Bitmap
        Dim road As Bitmap = img_watershed.Clone
        For y = 0 To img_watershed.Height - 1
            For x = 0 To img_watershed.Width - 1
                Dim c As Color = img_watershed.GetPixel(x, y)
                Dim c2 As Color = original.GetPixel(x, y)
                If Val(c.B) = 255 And c.R = 0 And c.G = 0 Then
                    road.SetPixel(x, y, c2)
                Else
                    road.SetPixel(x, y, Color.Black)
                End If
            Next
        Next
        Return road
    End Function
    Function watershed(ByVal img_marker As Bitmap, ByRef stage As Integer) As Bitmap
        Dim img_marker_temp As Bitmap = img_marker.Clone
        For water = 0 To 255 \ (256 / stage)
            Dim map_bfs(img_marker.Width, img_marker.Height) As Integer
            Dim que(1) As Queue
            que(0) = New Queue : que(1) = New Queue
            '------掃描圖片，將marker記錄在BFS中------
            'map_bfs=0:可以搜尋
            'map_bfs=1:馬路
            'map_bfs=2:天空
            For y = 0 To img_marker.Height - 1
                For x = 0 To img_marker.Width - 1
                    Dim c As Color = img_marker.GetPixel(x, y)
                    If Val(c.B) = 255 And c.R = 0 And c.G = 0 Then
                        map_bfs(x, y) = 1
                        que(0).Enqueue(New Point(x, y))
                    End If
                    If Val(c.R) = 255 And c.B = 0 And c.G = 0 Then
                        map_bfs(x, y) = 2
                        que(1).Enqueue(New Point(x, y))
                    End If
                Next
            Next

            '------BFS搜尋(t=0代表馬路的搜尋;t=1代表天空的搜尋)---------
            For t = 0 To 1
                Do
                    Dim dot As Point = que(t).Dequeue
                    Dim dx() As Integer = {0, 1, 0, -1}
                    Dim dy() As Integer = {-1, 0, 1, 0}
                    For i = 0 To UBound(dx)
                        Dim x As Integer = dot.X + dx(i)
                        Dim y As Integer = dot.Y + dy(i)
                        If x >= 0 And y >= 0 And x < img_marker.Width And y < img_marker.Height Then
                            If map_sobel(x, y) \ (256 / stage) <= water Then
                                If map_bfs(x, y) = 0 Then
                                    que(t).Enqueue(New Point(x, y))
                                    If t = 0 Then img_marker_temp.SetPixel(x, y, Color.Blue)
                                    If t = 1 Then img_marker_temp.SetPixel(x, y, Color.FromArgb(255, 0, 0))
                                    map_bfs(x, y) = t + 1
                                End If
                            End If
                        End If
                    Next
                Loop Until que(t).Count = 0
            Next
            PictureBox1.Image = img_marker_temp
            img_marker = img_marker_temp
            '  If water Mod 10 = 0 Then MsgBox("水位值:" & water)
        Next
        Return img_marker_temp
    End Function
    Function auto_marker(ByVal img_lbp As Bitmap) As Bitmap
        Dim block_road() As Integer = search_roadLBP(img_lbp) '馬路的材質標準
        Dim marker As Bitmap = img_lbp.Clone '將lbp上色用
        Dim map_erosion(img_lbp.Width \ W - 1, img_lbp.Height \ L - 1) As Integer
        For y = 0 To img_lbp.Height \ L - 1
            For x = 0 To img_lbp.Width \ W - 1
                '-----紀錄block的roi，用來判斷與馬路LBP相似度
                Dim roi(255) As Integer
                For i = y * L To y * L + (L - 1)
                    For j = x * W To x * W + (W - 1)
                        roi(map_lbp(j, i)) += 1
                    Next
                Next
                '---------------------------------
                If check(block_road, roi) Then '與馬路LBP相似，畫上marker
                    map_erosion(x, y) = 1
                    For i = y * L To y * L + (L - 1)
                        For j = x * W To x * W + (W - 1)
                            marker.SetPixel(j, i, Color.Blue)
                        Next
                    Next
                End If
            Next
        Next

        PictureBox1.Image = marker : MsgBox("與馬路相似的block")

        marker = img_lbp.Clone

        Dim map_erosion_temp(,) As Integer = map_erosion.Clone
        '--------侵蝕---------------
        For y = 0 To marker.Height / L - 1
            For x = 0 To marker.Width / W - 1
                If map_erosion(x, y) = 0 Then
                    Try
                        map_erosion_temp(x, y - 1) = 0
                        map_erosion_temp(x, y + 1) = 0
                        map_erosion_temp(x - 1, y) = 0
                        map_erosion_temp(x + 1, y) = 0
                    Catch ex As Exception
                    End Try
                End If
            Next
        Next

        '---------重新畫上侵蝕後的marker-----------
        For y = 0 To img_lbp.Height \ L - 1
            For x = 0 To img_lbp.Width \ W - 1
                If map_erosion_temp(x, y) = 1 Then
                    For i = y * L To y * L + (L - 1)
                        For j = x * W To x * W + (W - 1)
                            marker.SetPixel(j, i, Color.Blue)
                        Next
                    Next
                End If
            Next
        Next
        PictureBox1.Image = marker : MsgBox("侵蝕一個block後")

        '---------天空marker--------------
        For x = 0 To img_lbp.Width - 1
            marker.SetPixel(x, 0, Color.FromArgb(255, 0, 0))
        Next
        For y = 0 To 100
            marker.SetPixel(0, y, Color.FromArgb(255, 0, 0))
            marker.SetPixel(img_lbp.Width - 1, y, Color.FromArgb(255, 0, 0))
        Next
        Return marker
    End Function

    Function search_roadLBP(ByVal img_lbp As Bitmap) As Integer() '尋找馬路材質
        Dim block_road() As Integer = Nothing
        Dim Sy As Integer = img_lbp.Height \ L - 1  '以最底下一列來尋找馬路材質
        Dim max As Integer = 0
        For i = 0 To img_lbp.Width \ W - 1
            Dim compare(sample) As Integer
            '------先抓取一塊樣本跟其他做比較-----
            Dim roi(255) As Integer
            For iy = Sy * L To Sy * L + (L - 1)
                For ix = i * W To i * W + (W - 1)
                    roi(map_lbp(ix, iy)) += 1
                Next
            Next
            compare = findMaxIndex(roi)
            '-------------逐列搜尋-------------
            Dim cnt As Integer = 0
            For j = 0 To img_lbp.Width \ W - 1
                ReDim roi(255)
                For jy = Sy * L To Sy * L + (L - 1)
                    For jx = j * W To j * W + (W - 1)
                        roi(map_lbp(jx, jy)) += 1
                    Next
                Next
                If check(compare, roi) Then cnt += 1
            Next
            '-------若該block出現最多次，把它當成馬路材質-------
            If cnt >= max Then
                max = cnt
                block_road = compare
            End If
        Next
        Return block_road
    End Function
    Function check(compare() As Integer, roi() As Integer) As Boolean '容錯檢查，檢查兩BLOCK是否相似
        Dim c As Integer = fault '容錯值
        For i = 0 To UBound(compare)
            If c = 0 Then Return False
            If Array.IndexOf(compare, findMaxIndex(roi)(i)) = -1 Then
                c -= 1
            End If
        Next
        Return True
    End Function
    Function findMaxIndex(roi() As Integer) As Integer() '找出使用者輸入多少"精度"個的最大值
        Dim index(sample) As Integer
        Dim max As Integer = 0

        For i = 0 To UBound(index)
            index(i) = -1
        Next
        For t = 0 To UBound(index)
            max = 0
            For i = 0 To 255
                If roi(i) > max And Array.IndexOf(index, i) = -1 Then
                    max = roi(i)
                    index(t) = i
                End If
            Next
        Next
        Return index
    End Function

End Class
