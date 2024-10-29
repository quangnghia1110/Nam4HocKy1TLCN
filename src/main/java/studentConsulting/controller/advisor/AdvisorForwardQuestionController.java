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
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorForwardQuestionService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorForwardQuestionController {

    @Autowired
    private IAdvisorForwardQuestionService forwardQuestionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/forward-question/list")
    public ResponseEntity<DataResponse<Page<ForwardQuestionDTO>>> getForwardQuestions(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Integer toDepartmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionByRole(
                title, toDepartmentId, startDate, endDate, pageable, user.getId(), departmentId, isAdmin, isAdvisor);

        if (forwardQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ForwardQuestionDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy câu hỏi chuyển tiếp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ForwardQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách câu hỏi chuyển tiếp thành công")
                        .data(forwardQuestions)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/forward-question/update")
    public DataResponse<ForwardQuestionDTO> updateForwardQuestion(
            @RequestParam Integer forwardQuestionId,
            @RequestBody UpdateForwardQuestionRequest forwardQuestionRequest,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

        ForwardQuestionDTO updatedForwardQuestion = forwardQuestionService.updateForwardQuestionByRole(
                forwardQuestionId, forwardQuestionRequest, user.getId(), departmentId, isAdmin, isAdvisor);

        return DataResponse.<ForwardQuestionDTO>builder()
                .status("success")
                .message("Cập nhật câu hỏi chuyển tiếp thành công.")
                .data(updatedForwardQuestion)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/forward-question/delete")
    public DataResponse<Void> deleteForwardQuestion(
            @RequestParam Integer forwardQuestionId,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

        forwardQuestionService.deleteForwardQuestionByRole(forwardQuestionId, user.getId(), departmentId, isAdmin, isAdvisor);

        return DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu hỏi chuyển tiếp thành công.")
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/forward-question/detail")
    public ResponseEntity<DataResponse<ForwardQuestionDTO>> getForwardQuestionDetail(
            @RequestParam Integer forwardQuestionId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

        ForwardQuestionDTO forwardQuestionDTO = forwardQuestionService.getForwardQuestionDetailByRole(
                forwardQuestionId, user.getId(), departmentId, isAdmin, isAdvisor);

        if (forwardQuestionDTO == null) {
            throw new ErrorException("Không tìm thấy câu hỏi chuyển tiếp");
        }

        return ResponseEntity.ok(
                DataResponse.<ForwardQuestionDTO>builder()
                        .status("success")
                        .message("Lấy chi tiết câu hỏi chuyển tiếp thành công")
                        .data(forwardQuestionDTO)
                        .build()
        );
    }
}
