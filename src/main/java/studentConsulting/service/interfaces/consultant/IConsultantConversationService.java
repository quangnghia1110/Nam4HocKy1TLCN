package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.user.EmailDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;

import java.time.LocalDate;
import java.util.List;

public interface IConsultantConversationService {
    ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user);

    ConversationDTO approveMembersByEmail(Integer groupId, List<String> emailsToApprove);

    void deleteConversation(Integer conversationId);

    void updateConversationName(Integer conversationId, String newName);

    void removeMemberFromConversation(Integer conversationId, Integer userId);

    List<MemberDTO> findNonConsultantMembers(Integer conversationId);

    List<EmailDTO> findAllUsersWithRoleUser();

}
