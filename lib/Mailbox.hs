
import Control.Concurrent.Chan.Unagi


data MailBox a = MkMailBox
                 { mbInChan  :: InChan  a
                 , mbOutChan :: OutChan a
                 }

newMailBox :: IO (MailBox a)
newMailBox = uncurry MkMailBox <$> newChan

readMailBox :: MailBox a -> IO a
readMailBox = readChan . mbOutChan

writeMailBox :: MailBox a -> a -> IO ()
writeMailBox box = writeChan (mbInChan box)
