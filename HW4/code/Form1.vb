Public Class Form1
    Dim original As Bitmap '原圖
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If OpenFileDialog1.ShowDialog = DialogResult.OK Then
            PictureBox1.Image = Image.FromFile(OpenFileDialog1.FileName)
            original = PictureBox1.Image
        End If
    End Sub

    Dim bm_lbp As Bitmap
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click 'LBP轉換
        Dim roi(255) As Integer
        Dim lbp As Bitmap = lbpTransform(PictureBox1.Image, roi)
        write_chart(roi)
        PictureBox2.Image = lbp
        bm_lbp = lbp
        'lbp.Save("LBP.bmp")
    End Sub
    Sub write_chart(ByRef roi() As Integer) '將roi值列在chart上
        Chart1.Series(0).Points.Clear()
        For i = 0 To UBound(roi)
            Chart1.Series(0).Points.AddXY(i, roi(i))
        Next
    End Sub

    Dim pick(10) As Integer '判斷馬路LBP相似度，記錄馬路LBP最大值
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click '尋找
        '-------------------------功能1:掃描整張圖片，找出與馬路lbp相似的block---------------------------------------
        Dim bm As Bitmap = bm_lbp.Clone
        Dim img As Bitmap = bm.Clone '將lbp上色用
        Dim W As Integer = Val(TextBox4.Text)
        Dim L As Integer = Val(TextBox1.Text)
        Dim block(bm.Width \ W - 1, bm.Height \ L - 1) As Integer
        For y = 0 To bm.Height \ L - 1
            For x = 0 To bm.Width \ W - 1
                '-----紀錄block的roi，用來判斷與馬路LBP相似度
                Dim roi(255) As Integer
                For i = y * L To y * L + (L - 1)
                    For j = x * W To x * W + (W - 1)
                        roi(map_lbp(j, i)) += 1
                    Next
                Next
                '---------------------------------
                If check(pick, roi) Then '與馬路LBP相似，畫上marker
                    block(x, y) = 1
                    For i = y * L To y * L + (L - 1)
                        For j = x * W To x * W + (W - 1)
                            img.SetPixel(j, i, Color.Blue)
                        Next
                    Next
                End If
            Next
        Next
        PictureBox1.Image = img
        img = bm.Clone
        Dim block_temp(,) As Integer = block.Clone
        '--------方法1: 侵蝕---------------
        For y = 0 To bm.Height / L - 1
            For x = 0 To bm.Width / W - 1
                If block(x, y) = 0 Then
                    Try
                        block_temp(x, y - 1) = 0
                        block_temp(x, y + 1) = 0
                        block_temp(x - 1, y) = 0
                        block_temp(x + 1, y) = 0
                    Catch ex As Exception
                    End Try
                End If
            Next
        Next

        '---------重新畫上侵蝕後的marker-----------
        For y = 0 To bm.Height \ L - 1
            For x = 0 To bm.Width \ W - 1
                If block_temp(x, y) = 1 Then
                    For i = y * L To y * L + (L - 1)
                        For j = x * W To x * W + (W - 1)
                            img.SetPixel(j, i, Color.Blue)
                        Next
                    Next
                End If
            Next
        Next

        '---------天空marker--------------
        For x = 0 To bm.Width - 1
            img.SetPixel(x, 0, Color.FromArgb(255, 0, 0))
        Next
        For y = 0 To 100
            img.SetPixel(0, y, Color.FromArgb(255, 0, 0))
            img.SetPixel(bm.Width - 1, y, Color.FromArgb(255, 0, 0))
        Next

        '----------------------------方法2: 從馬路lbp block做BFS向外搜尋-----------------------------------
        'img = bm.Clone
        'Dim que As New Queue
        'que.Enqueue(New Point(Sx, Sy))
        'block(Sx, Sy) = 2
        'Do
        '    Dim dot As Point = que.Dequeue
        '    Dim dx() As Integer = {0, 1, 0, -1}
        '    Dim dy() As Integer = {-1, 0, 1, 0}

        '    For i = 0 To UBound(dx)
        '        Dim x As Integer = dot.X + dx(i)
        '        Dim y As Integer = dot.Y + dy(i)
        '        If x >= 0 And y >= 0 And x <= bm.Width / W - 1 And y <= bm.Height / L - 1 Then
        '            If block(x, y) = 0 Then Exit For

        '            If i = UBound(dx) Then
        '                For j = 0 To UBound(dx)
        '                    x = dot.X + dx(j)
        '                    y = dot.Y + dy(j)
        '                    If x >= 0 And y >= 0 And x <= bm.Width / W - 1 And y <= bm.Height / L - 1 Then
        '                        If block(x, y) = 1 Then
        '                            que.Enqueue(New Point(x, y))
        '                            block(x, y) = 2
        '                        End If
        '                    End If
        '                Next

        '                For j = dot.Y * L To dot.Y * L + (L - 1)
        '                    For k = dot.X * W To dot.X * W + (W - 1)
        '                        img.SetPixel(k, j, Color.Blue)
        '                    Next
        '                Next
        '            End If
        '        End If
        '    Next

        'Loop Until que.Count = 0
        PictureBox2.Image = img
    End Sub
    Function check(compare() As Integer, roi() As Integer) As Boolean '容錯檢查，檢查兩BLOCK是否相似
        Dim c As Integer = Val(TextBox3.Text) '容錯值
        For i = 0 To UBound(compare)
            If c = 0 Then Return False
            If Array.IndexOf(compare, findMaxIndex(roi)(i)) = -1 Then
                c -= 1
            End If
        Next
        Return True
    End Function

    Function findMaxIndex(roi() As Integer) As Integer() '找出使用者輸入多少"精度"個的最大值
        Dim index(Val(TextBox2.Text)) As Integer
        Dim max As Integer = 0

        For i = 0 To UBound(index)
            index(i) = -1
        Next
        For t = 0 To UBound(pick)
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

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click '劃出分水嶺
        Dim bm1 As Bitmap = PictureBox1.Image.Clone '水位255的影像
        Dim map_bfs(bm1.Width, bm1.Height) As Integer
        mix = original.Clone
        Dim que(1) As Queue
        que(0) = New Queue : que(1) = New Queue
        For y = 0 To bm1.Height - 1
            For x = 0 To bm1.Width - 1
                Dim c As Color = bm1.GetPixel(x, y)
                If Val(c.B) = 255 And Not (c.R = c.G And c.G = c.B) Then
                    map_bfs(x, y) = 1
                    que(0).Enqueue(New Point(x, y))
                End If
                If Val(c.R) = 255 And Not (c.R = c.G And c.G = c.B) Then
                    map_bfs(x, y) = 2
                    que(1).Enqueue(New Point(x, y))
                End If
            Next
        Next

        For t = 1 To 0 Step -1
            Do
                Dim dot As Point = que(t).Dequeue
                Dim dx() As Integer = {0, 1, 0, -1}
                Dim dy() As Integer = {-1, 0, 1, 0}
                For i = 0 To UBound(dx)
                    Dim x As Integer = dot.X + dx(i)
                    Dim y As Integer = dot.Y + dy(i)
                    If x >= 0 And y >= 0 And x < mix.Width And y < mix.Height Then
                        If map_bfs(x, y) = 2 - t Then
                            mix.SetPixel(x, y, Color.FromArgb(255, 0, 255))
                        End If
                    End If
                Next
            Loop Until que(t).Count = 0
        Next
        PictureBox2.Image = mix
        '  Form2.Show()
    End Sub
    Dim p As Graphics
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        p = PictureBox2.CreateGraphics
        original = PictureBox1.Image
    End Sub

    '紀錄滑鼠點擊的起始座標，方便後續BFS
    Dim Sx As Integer
    Dim Sy As Integer
    Private Sub PictureBox2_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox2.MouseDown '手動建立馬路block
        ReDim pick(Val(TextBox2.Text))
        Dim W As Integer = Val(TextBox4.Text)
        Dim L As Integer = Val(TextBox1.Text)
        Sx = e.X \ W
        Sy = e.Y \ L
        p.DrawRectangle(Pens.Red, Sx * W, Sy * L, W, L)
        Dim roi(255) As Integer
        For i = Sy * L To Sy * L + (L - 1)
            For j = Sx * W To Sx * W + (W - 1)
                roi(map_lbp(j, i)) += 1
            Next
        Next

        write_chart(roi)
        pick = findMaxIndex(roi)
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim bm1 As Bitmap = PictureBox1.Image
        mix = sobelTransform(bm1, 0)
        'mix.Save("sobel.bmp")
        Form2.Show()
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        For water = 50 To 255
            'water += Val(TextBox5.Text)
            Dim bm2 As Bitmap = PictureBox2.Image
            Dim img_marker As Bitmap = bm2.Clone
            Dim img_original As Bitmap = original.Clone
            Dim map_bfs(bm2.Width, bm2.Height) As Integer
            Dim que(1) As Queue
            que(0) = New Queue : que(1) = New Queue
            '------掃描圖片，將marker記錄在BFS中------
            'map_bfs=0:可以搜尋
            'map_bfs=1:馬路
            'map_bfs=2:天空
            For y = 0 To bm2.Height - 1
                For x = 0 To bm2.Width - 1
                    Dim c As Color = bm2.GetPixel(x, y)
                    If Val(c.B) = 255 And Not (c.R = c.G And c.G = c.B) Then
                        map_bfs(x, y) = 1
                        que(0).Enqueue(New Point(x, y))
                    End If
                    If Val(c.R) = 255 And Not (c.R = c.G And c.G = c.B) Then
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
                        If x >= 0 And y >= 0 And x < bm2.Width And y < bm2.Height Then
                            If map_sobel(x, y) <= water Then
                                If map_bfs(x, y) = 0 Then
                                    que(t).Enqueue(New Point(x, y))
                                    If t = 0 Then img_marker.SetPixel(x, y, Color.Blue)
                                    If t = 1 Then img_marker.SetPixel(x, y, Color.FromArgb(255, 0, 0))
                                    map_bfs(x, y) = t + 1
                                End If
                            End If
                        End If
                    Next
                Loop Until que(t).Count = 0
            Next
            PictureBox2.Image = img_marker
            If water Mod 10 = 0 Then MsgBox("水位值:" & water)
            If CheckBox1.Checked Then mix = sobelTransform(original, water)
        Next
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click '找出馬路LBP的block
        '從最底下一列尋找，找出出現最多次LBP的block來當馬路材質
        ReDim pick(Val(TextBox2.Text))
        Dim W As Integer = Val(TextBox4.Text)
        Dim L As Integer = Val(TextBox1.Text)
        Sy = bm_lbp.Height \ L - 1  '以最底下一列來尋找馬路材質
        Dim max As Integer = 0
        For i = 0 To bm_lbp.Width \ W - 1
            Dim compareA(Val(TextBox2.Text)) As Integer
            '------先抓取一塊樣本跟其他做比較-----
            Dim roi(255) As Integer
            For iy = Sy * L To Sy * L + (L - 1)
                For ix = i * W To i * W + (W - 1)
                    roi(map_lbp(ix, iy)) += 1
                Next
            Next
            compareA = findMaxIndex(roi)
            '-------------逐列搜尋-------------
            Dim cnt As Integer = 0
            For j = 0 To bm_lbp.Width \ W - 1
                ReDim roi(255)
                For jy = Sy * L To Sy * L + (L - 1)
                    For jx = j * W To j * W + (W - 1)
                        roi(map_lbp(jx, jy)) += 1
                    Next
                Next
                If check(compareA, roi) Then cnt += 1
            Next
            '-------若該block出現最多次，把它當成馬路材質-------
            If cnt >= max Then
                max = cnt
                pick = compareA
                Sx = i
            End If
        Next
        p.DrawRectangle(Pens.Red, Sx * W, Sy * L, W, L)
    End Sub
End Class
