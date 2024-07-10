package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.entity.main.questionEntity;

public interface notificationRepository extends  JpaRepository<questionEntity, Integer>{

}
