package studentConsulting.service;

import java.util.List;

import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.NotificationResponseDTO.NotificationDTO;

public interface INotificationService {
    public void sendNotification(NotificationDTO notificationDTO);
    List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId);

}
