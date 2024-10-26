package studentConsulting.controller.advisor;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConsultationScheduleService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonPdfService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdvisorConsultationScheduleController {

    @Autowired
    private IAdvisorConsultationScheduleService consultationScheduleService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/consultation-schedule/create")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultationSchedule(
            @RequestBody ManageCreateConsultantScheduleRequest request,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();
        Integer userId = user.getId();

        ConsultationScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Tạo buổi tư vấn thành công.")
                .data(consultationSchedule)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/consultation-schedule/update", consumes = {"multipart/form-data"})
    public DataResponse<ConsultationScheduleDTO> updateConsultationSchedule(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("consultationDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate consultationDate,
            @RequestParam("consultationTime") String consultationTime,
            @RequestParam("location") String location,
            @RequestParam("link") String link,
            @RequestParam("mode") Boolean mode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestParam("statusConfirmed") Boolean statusConfirmed,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        boolean isAdmin = SecurityConstants.Role.ADMIN.equals(role);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        UpdateConsultationScheduleRequest scheduleRequest = UpdateConsultationScheduleRequest.builder()
                .title(title)
                .content(content)
                .consultationDate(consultationDate)
                .consultationTime(consultationTime)
                .location(location)
                .link(link)
                .mode(mode)
                .statusPublic(statusPublic)
                .statusConfirmed(statusConfirmed)
                .build();

        ConsultationScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, departmentId, isAdmin, scheduleRequest, role, user.getId());

        return DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Cập nhật lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/consultation-schedule/delete")
    public ResponseEntity<DataResponse<Void>> deleteConsultationSchedule(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        consultationScheduleService.deleteConsultationSchedule(scheduleId, departmentId, user.getId(), role);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/consultation-schedule/list-member-join")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleRegistrationMemberDTO>>> getMembersByConsultationSchedule(
            @RequestParam Integer consultationScheduleId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "registeredAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleRegistrationMemberDTO> members;

        if (isAdmin) {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, null);
        } else {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, user.getId());
        }

        if (members.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                    .status("error").message("Không tìm thấy thành viên nào tham gia buổi tư vấn này.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                .status("success").message("Lấy danh sách thành viên thành công.").data(members).build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/consultation-schedule/detail")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> getConsultationScheduleDetail(
            @RequestParam("scheduleId") Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer userId = user.getId();

        ConsultationScheduleDTO scheduleDTO = consultationScheduleService.getConsultationScheduleByRole(scheduleId, role, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Lấy chi tiết lịch tư vấn thành công.")
                .data(scheduleDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-consultation-schedule-csv")
    public void exportConsultationSchedulesToCsv(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleDTO> consultationSchedules = consultationScheduleService.getAllConsultationSchedulesWithFilters(
                title, null, null, null, startDate, endDate, pageable);

        if (consultationSchedules.isEmpty()) {
            throw new ErrorException("Không có lịch tư vấn nào để xuất.");
        }

        List<String> headers = List.of("ID", "Title", "Content", "Consultant Name", "Consultation Date", "Consultation Time", "Location", "Link", "Mode", "Public", "Confirmed", "Department ID", "Created By");
        List<List<String>> data = consultationSchedules.getContent().stream()
                .map(schedule -> List.of(
                        schedule.getId() != null ? schedule.getId().toString() : "N/A",
                        schedule.getTitle() != null ? schedule.getTitle() : "N/A",
                        schedule.getContent() != null ? schedule.getContent() : "N/A",
                        schedule.getConsultantName() != null ? schedule.getConsultantName() : "N/A",
                        schedule.getConsultationDate() != null ? schedule.getConsultationDate().toString() : "N/A",
                        schedule.getConsultationTime() != null ? schedule.getConsultationTime() : "N/A",
                        schedule.getLocation() != null ? schedule.getLocation() : "N/A",
                        schedule.getLink() != null ? schedule.getLink() : "N/A",
                        schedule.getMode() != null ? schedule.getMode().toString() : "N/A",
                        schedule.getStatusPublic() != null ? schedule.getStatusPublic().toString() : "N/A",
                        schedule.getStatusConfirmed() != null ? schedule.getStatusConfirmed().toString() : "N/A",
                        schedule.getDepartment() != null ? schedule.getDepartment().getId().toString() : "N/A",
                        schedule.getCreatedBy() != null ? schedule.getCreatedBy().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "ConsultationSchedules_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Consultation Schedules", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-consultation-schedule-pdf")
    public void exportConsultationSchedulesToPdf(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleDTO> consultationSchedules = consultationScheduleService.getAllConsultationSchedulesWithFilters(
                title, null, null, null, startDate, endDate, pageable);

        if (consultationSchedules.isEmpty()) {
            throw new IOException("Không có lịch tư vấn nào để xuất.");
        }

        String templatePath = "/templates/consultation_schedule_template.html";
        String dataRows = buildConsultationScheduleDataRows(consultationSchedules.getContent());

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{schedules}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "ConsultationSchedules_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
        }

        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }

    private String buildConsultationScheduleDataRows(List<ConsultationScheduleDTO> schedules) {
        StringBuilder dataRows = new StringBuilder();

        for (ConsultationScheduleDTO schedule : schedules) {
            dataRows.append("<tr>")
                    .append("<td>").append(schedule.getId() != null ? schedule.getId() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getTitle() != null ? schedule.getTitle() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getContent() != null ? schedule.getContent() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getConsultantName() != null ? schedule.getConsultantName() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getConsultationDate() != null ? schedule.getConsultationDate() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getConsultationTime() != null ? schedule.getConsultationTime() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getLocation() != null ? schedule.getLocation() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getLink() != null ? schedule.getLink() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getMode() != null ? schedule.getMode().toString() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getStatusPublic() != null ? schedule.getStatusPublic().toString() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getStatusConfirmed() != null ? schedule.getStatusConfirmed().toString() : "N/A").append("</td>")
                    .append("<td>").append(schedule.getCreatedBy() != null ? schedule.getCreatedBy() : "N/A").append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-consultation-schedule-csv")
    public ResponseEntity<?> importConsultationSchedulesFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        consultationScheduleService.importConsultationSchedules(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-consultation-schedule-owner-csv")
    public void exportConsultantSchedulesToCsv(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ManageConsultantScheduleDTO> consultantSchedules = consultationScheduleService.getAllConsultationsWithFilters(
                title, null, null, null, startDate, endDate, pageable);

        if (consultantSchedules.isEmpty()) {
            throw new IOException("Không có lịch tư vấn nào để xuất.");
        }

        List<String> headers = List.of("ID", "Title", "Content", "Consultation Date", "Consultation Time", "Location", "Link", "Mode", "Public", "Confirmed", "Created By");
        List<List<String>> data = consultantSchedules.getContent().stream()
                .map(schedule -> List.of(
                        schedule.getId() != null ? schedule.getId().toString() : "N/A",
                        schedule.getTitle() != null ? schedule.getTitle() : "N/A",
                        schedule.getContent() != null ? schedule.getContent() : "N/A",
                        schedule.getConsultationDate() != null ? schedule.getConsultationDate().toString() : "N/A",
                        schedule.getConsultationTime() != null ? schedule.getConsultationTime() : "N/A",
                        schedule.getLocation() != null ? schedule.getLocation() : "N/A",
                        schedule.getLink() != null ? schedule.getLink() : "N/A",
                        schedule.getMode() != null ? schedule.getMode().toString() : "N/A",
                        schedule.getStatusPublic() != null ? schedule.getStatusPublic().toString() : "N/A",
                        schedule.getStatusConfirmed() != null ? schedule.getStatusConfirmed().toString() : "N/A",
                        schedule.getCreated_by() != null ? schedule.getCreated_by().toString() : "N/A" // Thêm trường createdBy
                ))
                .collect(Collectors.toList());

        String fileName = "ConsultantSchedules_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Consultant Schedules", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-consultation-schedule-owner-pdf")
    public void exportConsultantSchedulesToPdf(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ManageConsultantScheduleDTO> consultantSchedules = consultationScheduleService.getAllConsultationsWithFilters(
                title, null, null, null, startDate, endDate, pageable);

        if (consultantSchedules.isEmpty()) {
            throw new IOException("Không có lịch tư vấn nào để xuất.");
        }

        String templatePath = "/templates/consultation_schedule_owner_template.html";
        String dataRows = buildConsultantScheduleDataRows(consultantSchedules.getContent());

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{schedules}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "ConsultationScheduleOwner_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
        }

        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }

    private String buildConsultantScheduleDataRows(List<ManageConsultantScheduleDTO> schedules) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageConsultantScheduleDTO schedule : schedules) {
            dataRows.append("<tr>")
                    .append("<td>").append(schedule.getId()).append("</td>")
                    .append("<td>").append(schedule.getTitle()).append("</td>")
                    .append("<td>").append(schedule.getContent()).append("</td>")
                    .append("<td>").append(schedule.getConsultationDate()).append("</td>")
                    .append("<td>").append(schedule.getConsultationTime()).append("</td>")
                    .append("<td>").append(schedule.getLocation()).append("</td>")
                    .append("<td>").append(schedule.getLink()).append("</td>")
                    .append("<td>").append(schedule.getMode()).append("</td>")
                    .append("<td>").append(schedule.getStatusPublic()).append("</td>")
                    .append("<td>").append(schedule.getStatusConfirmed()).append("</td>")
                    .append("<td>").append(schedule.getCreated_by()).append("</td>") // Thêm trường createdBy
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-consultation-schedule-owner-csv")
    public ResponseEntity<?> importConsultationScheduleOwnerFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        consultationScheduleService.importManageConsultantSchedules(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import lịch tư vấn  thành công.")
                .build());
    }

}
