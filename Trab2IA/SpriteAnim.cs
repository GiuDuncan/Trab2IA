using System.Drawing;

namespace Trab2IA
{
    public class SpriteAnim
    {
        int frame, frames;
        public readonly Bitmap sprite;

        /// <summary>
        /// Cria um bitmap a partir de uma imagem e a separa em frames para iterar e formar uma animação
        /// </summary>
        /// <param name="spritePath">Caminho para a imagem</param>
        /// <param name="frames">Número de frames na imagem</param>
        public SpriteAnim(string spritePath, int frames)
        {
            sprite = new Bitmap(spritePath);
            this.frames = frames;
        }

        public Rectangle SrcRect(int frame)
        {
            return new Rectangle((sprite.Width / frames) * (frame % frames), 0, sprite.Width / frames, sprite.Height);
        }
    }
}