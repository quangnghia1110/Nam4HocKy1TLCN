package studentConsulting.service.interfaces.common;

import java.time.LocalDateTime;
import java.util.Map;

public interface IStatusOnlineService {
    void updateStatus(String email, boolean isOnline);

    Map<String, LocalDateTime> getOnlineUsers();

}

