package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.communication.MessageDTO;

public interface IAdminMessageService {

    void deleteMessageById(Integer id);

    MessageDTO getMessageById(Integer id);

    Page<MessageDTO> getAllMessagesWithFilters(Integer conversationId, Pageable pageable);

}
