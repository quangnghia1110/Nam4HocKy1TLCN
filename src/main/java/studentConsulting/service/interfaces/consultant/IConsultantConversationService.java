package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;

import java.time.LocalDate;
import java.util.List;

public interface IConsultantConversationService {
    ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user);

    Page<ConversationDTO> findConversationsByConsultantWithFilters(
            Integer userId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConversationDTO findConversationById(Integer conversationId);

    ConversationDTO approveMember(Integer groupId, Integer userId);

    void deleteConversation(Integer conversationId);

    void updateConversationName(Integer conversationId, String newName);

    void removeMemberFromConversation(Integer conversationId, Integer userId);

    List<MemberDTO> findNonConsultantMembers(Integer conversationId);
}
