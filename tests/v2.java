public static ImageIcon getImageIcon(string path)
{
    if (path == null)
    {
        log.error("Icon path is null");
        return null;
    }

    java.net.URL imgURL = GuiImporter.class.getResource(path);

    if (imgURL == null)
    {
        log.error("Couldn't find icon:" + imgURL);
        return null;
    }
    else
        return new ImageIcon(imgURL);
}