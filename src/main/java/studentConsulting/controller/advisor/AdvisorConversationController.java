package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConversationService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorConversationController {

    @Autowired
    private IAdvisorConversationService conversationService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/conversation/list-consultant")
    public ResponseEntity<DataResponse<Page<ConversationDTO>>> getAdvisorConversations(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(
                departmentId, name, startDate, endDate, pageable);

        if (conversations.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConversationDTO>>builder().status("error")
                    .message("Không tìm thấy cuộc trò chuyện trong phòng ban.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder().status("success")
                .message("Lấy danh sách các cuộc trò chuyện thành công.").data(conversations).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PutMapping("/advisor/conversation/update-consultant")
    public ResponseEntity<?> updateAdvisorConversation(@RequestParam Integer conversationId,
                                                       @RequestParam String newName,
                                                       @RequestParam(required = false) Integer userIdToRemove,
                                                       Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        conversationService.updateConversationName(conversationId, newName, departmentId);

        if (userIdToRemove != null) {
            conversationService.removeMemberFromConversation(conversationId, userIdToRemove, departmentId);
        }

        return ResponseEntity.ok(
                DataResponse.<Void>builder()
                        .status("success")
                        .message("Cập nhật cuộc trò chuyện thành công.")
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @DeleteMapping("/advisor/conversation/delete-consultant")
    public ResponseEntity<?> deleteAdvisorConversation(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        conversationService.deleteConversation(conversationId, departmentId);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success")
                .message("Cuộc trò chuyện đã được xóa thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/conversation/detail")
    public ResponseEntity<DataResponse<ConversationDTO>> getConversationByIdAndDepartment(@RequestParam("id") Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        ConversationDTO conversationDTO = conversationService.getConversationByIdAndDepartment(conversationId, departmentId);
        if (conversationDTO == null) {
            throw new ErrorException("Không tìm thấy cuộc trò chuyện");
        }

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Lấy chi tiết cuộc trò chuyện thành công.")
                .data(conversationDTO)
                .build());
    }


}
