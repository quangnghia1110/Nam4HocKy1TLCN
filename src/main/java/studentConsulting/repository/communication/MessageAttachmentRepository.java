package studentConsulting.repository.communication;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.communication.MessageAttachmentEntity;

public interface MessageAttachmentRepository extends  JpaRepository<MessageAttachmentEntity, Integer>{

}
