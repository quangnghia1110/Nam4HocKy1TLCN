package studentConsulting.repository.communication;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.communication.MessageEntity;

public interface MessageRepository extends  JpaRepository<MessageEntity, Integer>{

}
