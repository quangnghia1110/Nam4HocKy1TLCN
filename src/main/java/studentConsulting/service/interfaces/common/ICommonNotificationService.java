package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO.NotificationDTO;

import java.util.List;

public interface ICommonNotificationService {
    void sendNotification(NotificationDTO notificationDTO);

    List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId);

}
