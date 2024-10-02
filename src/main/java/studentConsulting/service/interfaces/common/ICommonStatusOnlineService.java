package studentConsulting.service.interfaces.common;

import java.time.LocalDateTime;
import java.util.Map;

public interface ICommonStatusOnlineService {
    void updateStatus(String email, boolean isOnline);

    Map<String, LocalDateTime> getOnlineUsers();

}

