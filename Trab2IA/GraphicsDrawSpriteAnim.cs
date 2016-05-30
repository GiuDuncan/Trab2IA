using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Trab2IA
{
    static class GraphicsDrawSpriteAnim
    {
        public static int Frame = 0; //Frame se inicia em 0 e pode ser iterado
        public static void DrawSpriteAnim(this Graphics graphics, SpriteAnim spriteAnim, int x, int y, int width, int height)
        {
            var destRect = new Rectangle(x, y, width, height);
            graphics.DrawImage(spriteAnim.sprite,  destRect, spriteAnim.SrcRect(Frame), GraphicsUnit.Pixel);
        }
    }
}
