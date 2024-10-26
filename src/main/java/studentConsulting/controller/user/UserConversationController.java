package studentConsulting.controller.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.request.socket.CreateConversationUserRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.user.IUserConversationService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class UserConversationController {

    @Autowired
    private IUserConversationService conversationService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversation(
            @RequestBody CreateConversationUserRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO createdConversation = conversationService.createConversation(request, user);

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder().status("success")
                .message("Cuộc trò chuyện đã được tạo thành công.").data(createdConversation).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/list")
    public ResponseEntity<DataResponse<Page<ConversationDTO>>> getListConversationByRole(
            Principal principal,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer userId = user.getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.getListConversationByRole(userId, role, depId, name, startDate, endDate, pageable);

        Page<ConversationDTO> filteredConversations = conversations.map(conversation -> {
            if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
                boolean isMember = conversation.getMembers().stream()
                        .anyMatch(member -> member.getId().equals(userId));

                if (!isMember) {
                    throw new ErrorException("Bạn không có quyền truy cập vào cuộc trò chuyện này.");
                }
            } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
                if (!conversation.getDepartment().getId().equals(depId)) {
                    throw new ErrorException("Bạn không có quyền truy cập vào cuộc trò chuyện này vì không thuộc phòng ban của bạn.");
                }
            }
            return conversation;
        });

        if (filteredConversations.isEmpty()) {
            return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder()
                    .status("success")
                    .message("Không có cuộc trò chuyện nào.")
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder()
                .status("success")
                .message("Lấy danh sách cuộc trò chuyện thành công.")
                .data(filteredConversations)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/detail")
    public ResponseEntity<?> getDetailConversationByRole(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));

            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền truy cập vào cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền truy cập vào cuộc trò chuyện này vì không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Thông tin cuộc trò chuyện.")
                .data(conversation)
                .build());
    }

}
