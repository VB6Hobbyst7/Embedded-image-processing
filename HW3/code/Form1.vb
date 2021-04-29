    Dim original As Bitmap '原圖

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If OpenFileDialog1.ShowDialog = DialogResult.OK Then
            PictureBox1.Image = Image.FromFile(OpenFileDialog1.FileName)
            original = PictureBox1.Image.Clone
            manual_marker = PictureBox1.Image
            g = Graphics.FromImage(manual_marker)
        End If
    End Sub    
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim bm1 As Bitmap = PictureBox1.Image.Clone
        mix = sobelTransform(bm1, 0)
        'mix.Save("sobel.bmp")
        Form2.Show()
    End Sub

    Dim p As Graphics
    Dim g As Graphics
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        p = PictureBox2.CreateGraphics
        manual_marker = PictureBox1.Image
        g = Graphics.FromImage(manual_marker)
        original = PictureBox1.Image.Clone
    End Sub
    Dim manual_marker As Bitmap
    Dim key As Boolean
    Private Sub PictureBox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        key = True
    End Sub
    Private Sub PictureBox1_Mouseup(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseUp
        key = False
    End Sub
    Private Sub PictureBox1_Mousemove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        If key Then
            g.FillEllipse(Brushes.Blue, e.X, e.Y, 20, 20)
            PictureBox1.Image = manual_marker
        End If
    End Sub
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim bm1 As Bitmap = PictureBox1.Image
        Dim map_bfs(bm1.Width, bm1.Height) As Integer
        '------------計算有幾條marker-----------------------
        Dim cnt As Integer = 0

        For y = bm1.Height - 1 To 0 Step -1
            For x = 0 To bm1.Width - 1
                Dim c As Color = bm1.GetPixel(x, y)
                If Val(c.B) = 255 And c.R = 0 And c.G = 0 Then '發現手動的marker
                    cnt += 1
                    Dim que_search As New Queue
                    que_search.Enqueue(New Point(x, y))
                    bm1.SetPixel(x, y, Color.Black)
                    map_bfs(x, y) = cnt

                    Dim dx() As Integer = {0, 1, 0, -1}
                    Dim dy() As Integer = {-1, 0, 1, 0}
                    Do
                        Dim p As Point = que_search.Dequeue
                        For i = 0 To UBound(dx)
                            Dim px As Integer = p.X + dx(i)
                            Dim py As Integer = p.Y + dy(i)
                            If px >= 0 And py >= 0 And px < bm1.Width And py < bm1.Height Then
                                c = bm1.GetPixel(px, py)
                                If Val(c.B) = 255 And c.R = 0 And c.G = 0 Then
                                    que_search.Enqueue(New Point(px, py))
                                    bm1.SetPixel(px, py, Color.Black)
                                    map_bfs(px, py) = cnt
                                End If
                            End If
                        Next
                    Loop Until que_search.Count = 0
                End If
            Next
        Next
        MsgBox(cnt)
        Dim bm2 As Bitmap = original.Clone
        For water = 50 To 255

            Dim que(cnt) As Queue
            For i = 1 To cnt
                que(i) = New Queue
                For y = 0 To bm1.Height - 1
                    For x = 0 To bm1.Width - 1
                        If map_bfs(x, y) = i Then
                            que(i).Enqueue(New Point(x, y))
                            'Select Case i
                            '    Case 1
                            '        bm2.SetPixel(x, y, Color.Red)
                            '    Case 2
                            '        bm2.SetPixel(x, y, Color.Blue)
                            '    Case 3
                            '        bm2.SetPixel(x, y, Color.Yellow)
                            'End Select
                        End If
                    Next
                Next

                Do
                    Dim p As Point = que(i).Dequeue
                    Dim dx() As Integer = {0, 1, 0, -1}
                    Dim dy() As Integer = {-1, 0, 1, 0}
                    For j = 0 To UBound(dx)
                        Dim px As Integer = p.X + dx(j)
                        Dim py As Integer = p.Y + dy(j)
                        If px >= 0 And py >= 0 And px < bm1.Width And py < bm1.Height Then
                            If map_sobel(px, py) <= water Then
                                If map_bfs(px, py) = 0 Then
                                    que(i).Enqueue(New Point(px, py))
                                    map_bfs(px, py) = i
                                    'Select Case i
                                    '    Case 1
                                    '        bm2.SetPixel(px, py, Color.Red)
                                    '    Case 2
                                    '        bm2.SetPixel(px, py, Color.Blue)
                                    '    Case 3
                                    '        bm2.SetPixel(px, py, Color.Yellow)
                                    'End Select
                                ElseIf map_bfs(px, py) <> i Then
                                    bm2.SetPixel(px, py, Color.Green)
                                End If
                            End If
                        End If
                    Next
                Loop Until que(i).Count = 0
            Next
            PictureBox2.Image = bm2
            ' If water Mod 10 = 0 Then MsgBox("水位值:" & water)
        Next
    End Sub
End Class
