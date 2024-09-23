package studentConsulting.service;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.model.payload.request.socket.CreateConversationUserRequest;

public interface IConversationService {
    ConversationDTO createConversation(CreateConversationUserRequest request, UserInformationEntity user);
    public Page<ConversationDTO> findConversationsByUserWithFilters(
            Integer userId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);
    public Page<ConversationDTO> findConversationsByConsultantWithFilters(
            Integer userId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);
    ConversationDTO findConversationById(Integer conversationId);

    public ConversationDTO approveMember(Integer groupId, Integer userId);
    public ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user);
    public void deleteConversation(Integer conversationId);
    
    public void updateConversationName(Integer conversationId, String newName);
    
    public void removeMemberFromConversation(Integer conversationId, Integer userId);
    
    public List<MemberDTO> findNonConsultantMembers(Integer conversationId);
}
