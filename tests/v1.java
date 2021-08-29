public static ImageIcon getImageIcon(string path)
{
    java.net.URL imgURL = GuiImporter.class.getResource(path);

    if (imgURL != null)
    {
        return new ImageIcon(imgURL);
    }
    else
    {
        log.error("Couldn't find icon:" + imgURL);
    }
        return null;
}