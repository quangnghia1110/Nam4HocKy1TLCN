package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.model.payload.mapper.admin.MessageMapper;
import studentConsulting.repository.communication.MessageRecallRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.service.interfaces.admin.IAdminMessageService;
import studentConsulting.specification.communication.MessageSpecification;

@Service
public class AdminMessageServiceImpl implements IAdminMessageService {

    @Autowired
    private MessageRepository messageRepository;

    @Autowired
    private MessageRecallRepository messageRecallRepository;

    @Autowired
    private MessageMapper messageMapper;

    @Override
    @Transactional
    public void deleteMessageById(Integer id) {
        MessageEntity message = messageRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Tin nhắn không tồn tại"));

        message.setRecalledForEveryone(true);
        messageRepository.save(message);
    }

    @Override
    public MessageDTO getMessageById(Integer id) {
        return messageRepository.findById(id)
                .map(message -> messageMapper.mapToDTO(message, message.getSender().getId()))
                .orElseThrow(() -> new ErrorException("Không tìm thấy tin nhắn"));
    }

    @Override
    public Page<MessageDTO> getAllMessagesWithFilters(Integer conversationId, Pageable pageable) {
        Specification<MessageEntity> spec = Specification.where(null);

        if (conversationId != null) {
            spec = spec.and(MessageSpecification.hasConversationId(conversationId));
        }

        return messageRepository.findAll(spec, pageable)
                .map(message -> messageMapper.mapToDTO(message, message.getSender().getId()));
    }
}
