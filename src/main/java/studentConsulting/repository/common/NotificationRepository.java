package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.notification.NotificationEntity;

import java.util.List;

@Repository
public interface NotificationRepository extends JpaRepository<NotificationEntity, Integer> {

    List<NotificationEntity> findByReceiverId(Integer receiverId);

}
