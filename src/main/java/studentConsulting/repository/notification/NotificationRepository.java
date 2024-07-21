package studentConsulting.repository.notification;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.notification.NotificationEntity;

public interface NotificationRepository extends  JpaRepository<NotificationEntity, Integer>{

}
