package studentConsulting.service.interfaces.user;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.request.socket.CreateConversationUserRequest;

import java.time.LocalDate;

public interface IUserConversationService {
    ConversationDTO createConversation(CreateConversationUserRequest request, UserInformationEntity user);

    Page<ConversationDTO> findConversationsByUserWithFilters(
            Integer userId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void deleteConversation(Integer conversationId);

    ConversationDTO findConversationById(Integer conversationId);

}
