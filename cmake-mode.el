;=============================================================================
; CMake - Cross Platform Makefile Generator
; Copyright 2000-2009 Kitware, Inc., Insight Software Consortium
;
; Distributed under the OSI-approved BSD License (the "License");
; see accompanying file Copyright.txt for details.
;
; This software is distributed WITHOUT ANY WARRANTY; without even the
; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; See the License for more information.
;=============================================================================
;;; cmake-mode.el --- major-mode for editing CMake sources

;------------------------------------------------------------------------------

;;; Commentary:

;; Provides syntax highlighting and indentation for CMakeLists.txt and
;; *.cmake source files.
;;
;; Add this code to your .emacs file to use the mode:
;;
;;  (setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
;;  (require 'cmake-mode)
;;  (setq auto-mode-alist
;;        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
;;                  ("\\.cmake\\'" . cmake-mode))
;;                auto-mode-alist))

;------------------------------------------------------------------------------

;;; Code:
;;
;; cmake executable variable used to run cmake --help-command
;; on commands in cmake-mode
;;
;; cmake-command-help Written by James Bigler 
;;

(defcustom cmake-mode-cmake-executable "cmake"
  "*The name of the cmake executable.

This can be either absolute or looked up in $PATH.  You can also
set the path with these commands:
 (setenv \"PATH\" (concat (getenv \"PATH\") \";C:\\\\Program Files\\\\CMake 2.8\\\\bin\"))
 (setenv \"PATH\" (concat (getenv \"PATH\") \":/usr/local/cmake/bin\"))"
  :type 'file
  :group 'cmake)
;;
;; Regular expressions used by line indentation function.
;;
(defconst cmake-regex-blank "^[ \t]*$")
(defconst cmake-regex-comment "#.*")
(defconst cmake-regex-paren-left "(")
(defconst cmake-regex-paren-right ")")
(defconst cmake-regex-argument-quoted
  "\"\\([^\"\\\\]\\|\\\\\\(.\\|\n\\)\\)*\"")
(defconst cmake-regex-argument-unquoted
  "\\([^ \t\r\n()#\"\\\\]\\|\\\\.\\)\\([^ \t\r\n()#\\\\]\\|\\\\.\\)*")
(defconst cmake-regex-token (concat "\\(" cmake-regex-comment
                                    "\\|" cmake-regex-paren-left
                                    "\\|" cmake-regex-paren-right
                                    "\\|" cmake-regex-argument-unquoted
                                    "\\|" cmake-regex-argument-quoted
                                    "\\)"))
(defconst cmake-regex-indented (concat "^\\("
                                       cmake-regex-token
                                       "\\|" "[ \t\r\n]"
                                       "\\)*"))
(defconst cmake-regex-block-open
  "^\\(IF\\|MACRO\\|FOREACH\\|ELSE\\|ELSEIF\\|WHILE\\|FUNCTION\\)$")
(defconst cmake-regex-block-close
  "^[ \t]*\\(ENDIF\\|ENDFOREACH\\|ENDMACRO\\|ELSE\\|ELSEIF\\|ENDWHILE\\|ENDFUNCTION\\)[ \t]*(")

;------------------------------------------------------------------------------

;;
;; Helper functions for line indentation function.
;;
(defun cmake-line-starts-inside-string ()
  "Determine whether the beginning of the current line is in a string."
  (if (save-excursion
        (beginning-of-line)
        (let ((parse-end (point)))
          (beginning-of-buffer)
          (nth 3 (parse-partial-sexp (point) parse-end))
          )
        )
      t
    nil
    )
  )

(defun cmake-find-last-indented-line ()
  "Move to the beginning of the last line that has meaningful indentation."
  (let ((point-start (point))
        region)
    (forward-line -1)
    (setq region (buffer-substring-no-properties (point) point-start))
    (while (and (not (bobp))
                (or (looking-at cmake-regex-blank)
                    (not (and (string-match cmake-regex-indented region)
                              (= (length region) (match-end 0))))))
      (forward-line -1)
      (setq region (buffer-substring-no-properties (point) point-start))
      )
    )
  )

;------------------------------------------------------------------------------

;;
;; Line indentation function.
;;
(defun cmake-indent ()
  "Indent current line as CMAKE code."
  (interactive)
  (if (cmake-line-starts-inside-string)
      ()
    (if (bobp)
        (cmake-indent-line-to 0)
      (let (cur-indent)

        (save-excursion
          (beginning-of-line)

          (let ((point-start (point))
                token)

            ; Search back for the last indented line.
            (cmake-find-last-indented-line)

            ; Start with the indentation on this line.
            (setq cur-indent (current-indentation))

            ; Search forward counting tokens that adjust indentation.
            (while (re-search-forward cmake-regex-token point-start t)
              (setq token (match-string 0))
              (if (string-match (concat "^" cmake-regex-paren-left "$") token)
                  (setq cur-indent (+ cur-indent cmake-tab-width))
                )
              (if (string-match (concat "^" cmake-regex-paren-right "$") token)
                  (setq cur-indent (- cur-indent cmake-tab-width))
                )
              (if (and
                   (string-match cmake-regex-block-open token)
                   (looking-at (concat "[ \t]*" cmake-regex-paren-left))
                   )
                  (setq cur-indent (+ cur-indent cmake-tab-width))
                )
              )
            (goto-char point-start)

            ; If this is the end of a block, decrease indentation.
            (if (looking-at cmake-regex-block-close)
                (setq cur-indent (- cur-indent cmake-tab-width))
              )
            )
          )

        ; Indent this line by the amount selected.
        (if (< cur-indent 0)
            (cmake-indent-line-to 0)
          (cmake-indent-line-to cur-indent)
          )
        )
      )
    )
  )

(defun cmake-point-in-indendation ()
  (string-match "^[ \\t]*$" (buffer-substring (point-at-bol) (point))))

(defun cmake-indent-line-to (column)
  "Indent the current line to COLUMN.
If point is within the existing indentation it is moved to the end of
the indentation.  Otherwise it retains the same position on the line"
  (if (cmake-point-in-indendation)
      (indent-line-to column)
    (save-excursion (indent-line-to column))))

;------------------------------------------------------------------------------

;;
;; Helper functions for buffer
;;
(defun unscreamify-cmake-buffer ()
  "Convert all CMake commands to lowercase in buffer."
  (interactive)
  (setq save-point (point))
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)\\(\\w+\\)\\([ \t]*(\\)" nil t)
    (replace-match 
     (concat 
      (match-string 1) 
      (downcase (match-string 2)) 
      (match-string 3)) 
     t))
  (goto-char save-point)
  )

;------------------------------------------------------------------------------

;;
;; Keyword highlighting regex-to-face map.
;;
(defconst cmake-font-lock-keywords
  (list '("^[ \t]*\\(add_custom_command\\|add_custom_target\\|add_definitions\\|add_dependencies\\|add_executable\\|add_library\\|add_subdirectory\\|add_test\\|aux_source_directory\\|break\\|build_command\\|cmake_minimum_required\\|cmake_policy\\|configure_file\\|create_test_sourcelist\\|define_property\\|else\\|elseif\\|enable_language\\|enable_testing\\|endforeach\\|endfunction\\|endif\\|endmacro\\|endwhile\\|execute_process\\|export\\|file\\|find_file\\|find_library\\|find_package\\|find_path\\|find_program\\|fltk_wrap_ui\\|foreach\\|function\\|get_cmake_property\\|get_directory_property\\|get_filename_component\\|get_property\\|get_source_file_property\\|get_target_property\\|get_test_property\\|if\\|include\\|include_directories\\|include_external_msproject\\|include_regular_expression\\|install\\|link_directories\\|list\\|load_cache\\|load_command\\|macro\\|mark_as_advanced\\|math\\|message\\|option\\|output_required_files\\|project\\|qt_wrap_cpp\\|qt_wrap_ui\\|remove_definitions\\|return\\|separate_arguments\\|set\\|set_directory_properties\\|set_property\\|set_source_files_properties\\|set_target_properties\\|set_tests_properties\\|site_name\\|source_group\\|string\\|target_link_libraries\\|try_compile\\|try_run\\|unset\\|variable_watch\\|while\\|ADD_CUSTOM_COMMAND\\|ADD_CUSTOM_TARGET\\|ADD_DEFINITIONS\\|ADD_DEPENDENCIES\\|ADD_EXECUTABLE\\|ADD_LIBRARY\\|ADD_SUBDIRECTORY\\|ADD_TEST\\|AUX_SOURCE_DIRECTORY\\|BREAK\\|BUILD_COMMAND\\|CMAKE_MINIMUM_REQUIRED\\|CMAKE_POLICY\\|CONFIGURE_FILE\\|CREATE_TEST_SOURCELIST\\|DEFINE_PROPERTY\\|ELSE\\|ELSEIF\\|ENABLE_LANGUAGE\\|ENABLE_TESTING\\|ENDFOREACH\\|ENDFUNCTION\\|ENDIF\\|ENDMACRO\\|ENDWHILE\\|EXECUTE_PROCESS\\|EXPORT\\|FILE\\|FIND_FILE\\|FIND_LIBRARY\\|FIND_PACKAGE\\|FIND_PATH\\|FIND_PROGRAM\\|FLTK_WRAP_UI\\|FOREACH\\|FUNCTION\\|GET_CMAKE_PROPERTY\\|GET_DIRECTORY_PROPERTY\\|GET_FILENAME_COMPONENT\\|GET_PROPERTY\\|GET_SOURCE_FILE_PROPERTY\\|GET_TARGET_PROPERTY\\|GET_TEST_PROPERTY\\|IF\\|INCLUDE\\|INCLUDE_DIRECTORIES\\|INCLUDE_EXTERNAL_MSPROJECT\\|INCLUDE_REGULAR_EXPRESSION\\|INSTALL\\|LINK_DIRECTORIES\\|LIST\\|LOAD_CACHE\\|LOAD_COMMAND\\|MACRO\\|MARK_AS_ADVANCED\\|MATH\\|MESSAGE\\|OPTION\\|OUTPUT_REQUIRED_FILES\\|PROJECT\\|QT_WRAP_CPP\\|QT_WRAP_UI\\|REMOVE_DEFINITIONS\\|RETURN\\|SEPARATE_ARGUMENTS\\|SET\\|SET_DIRECTORY_PROPERTIES\\|SET_PROPERTY\\|SET_SOURCE_FILES_PROPERTIES\\|SET_TARGET_PROPERTIES\\|SET_TESTS_PROPERTIES\\|SITE_NAME\\|SOURCE_GROUP\\|STRING\\|TARGET_LINK_LIBRARIES\\|TRY_COMPILE\\|TRY_RUN\\|UNSET\\|VARIABLE_WATCH\\|WHILE\\)\\b" 1 font-lock-keyword-face)
        '("^[ \t]*\\(\\w+\\)[ \t]*(" 1 font-lock-function-name-face)
        '("^[ \t]*\\(\\(end\\)?\\(function\\|macro\\)\\)(\\(\\w+\\)" 4 font-lock-function-name-face)
        '("${[A-Za-z0-0_\\{\\}$]+}" 0 font-lock-variable-name-face)
        '("\\b\\(ALLOW_DUPLICATE_CUSTOM_TARGETS\\|DEBUG_CONFIGURATIONS\\|DISABLED_FEATURES\\|ENABLED_FEATURES\\|ENABLED_LANGUAGES\\|FIND_LIBRARY_USE_LIB64_PATHS\\|FIND_LIBRARY_USE_OPENBSD_VERSIONING\\|GLOBAL_DEPENDS_DEBUG_MODE\\|GLOBAL_DEPENDS_NO_CYCLES\\|IN_TRY_COMPILE\\|PACKAGES_FOUND\\|PACKAGES_NOT_FOUND\\|PREDEFINED_TARGETS_FOLDER\\|REPORT_UNDEFINED_PROPERTIES\\|RULE_LAUNCH_COMPILE\\|RULE_LAUNCH_CUSTOM\\|RULE_LAUNCH_LINK\\|RULE_MESSAGES\\|TARGET_ARCHIVES_MAY_BE_SHARED_LIBS\\|TARGET_SUPPORTS_SHARED_LIBS\\|USE_FOLDERS\\|__CMAKE_DELETE_CACHE_CHANGE_VARS_\\|ADDITIONAL_MAKE_CLEAN_FILES\\|CACHE_VARIABLES\\|CLEAN_NO_CUSTOM\\|COMPILE_DEFINITIONS\\|COMPILE_DEFINITIONS_\\w+\\|DEFINITIONS\\|EXCLUDE_FROM_ALL\\|IMPLICIT_DEPENDS_INCLUDE_TRANSFORM\\|INCLUDE_DIRECTORIES\\|INCLUDE_REGULAR_EXPRESSION\\|INTERPROCEDURAL_OPTIMIZATION\\|INTERPROCEDURAL_OPTIMIZATION_\\w+\\|LINK_DIRECTORIES\\|LISTFILE_STACK\\|MACROS\\|PARENT_DIRECTORY\\|RULE_LAUNCH_COMPILE\\|RULE_LAUNCH_CUSTOM\\|RULE_LAUNCH_LINK\\|TEST_INCLUDE_FILE\\|VARIABLES\\|\\w+_OUTPUT_NAME\\|\\w+_POSTFIX\\|ARCHIVE_OUTPUT_DIRECTORY\\|ARCHIVE_OUTPUT_DIRECTORY_\\w+\\|ARCHIVE_OUTPUT_NAME\\|ARCHIVE_OUTPUT_NAME_\\w+\\|BUILD_WITH_INSTALL_RPATH\\|COMPILE_DEFINITIONS\\|COMPILE_DEFINITIONS_\\w+\\|COMPILE_FLAGS\\|DEBUG_POSTFIX\\|DEFINE_SYMBOL\\|ENABLE_EXPORTS\\|EXCLUDE_FROM_ALL\\|EchoString\\|FOLDER\\|FRAMEWORK\\|Fortran_MODULE_DIRECTORY\\|GENERATOR_FILE_NAME\\|HAS_CXX\\|IMPLICIT_DEPENDS_INCLUDE_TRANSFORM\\|IMPORTED\\|IMPORTED_CONFIGURATIONS\\|IMPORTED_IMPLIB\\|IMPORTED_IMPLIB_\\w+\\|IMPORTED_LINK_DEPENDENT_LIBRARIES\\|IMPORTED_LINK_DEPENDENT_LIBRARIES_\\w+\\|IMPORTED_LINK_INTERFACE_LANGUAGES\\|IMPORTED_LINK_INTERFACE_LANGUAGES_\\w+\\|IMPORTED_LINK_INTERFACE_LIBRARIES\\|IMPORTED_LINK_INTERFACE_LIBRARIES_\\w+\\|IMPORTED_LINK_INTERFACE_MULTIPLICITY\\|IMPORTED_LINK_INTERFACE_MULTIPLICITY_\\w+\\|IMPORTED_LOCATION\\|IMPORTED_LOCATION_\\w+\\|IMPORTED_NO_SONAME\\|IMPORTED_NO_SONAME_\\w+\\|IMPORTED_SONAME\\|IMPORTED_SONAME_\\w+\\|IMPORT_PREFIX\\|IMPORT_SUFFIX\\|INSTALL_NAME_DIR\\|INSTALL_RPATH\\|INSTALL_RPATH_USE_LINK_PATH\\|INTERPROCEDURAL_OPTIMIZATION\\|INTERPROCEDURAL_OPTIMIZATION_\\w+\\|LABELS\\|LIBRARY_OUTPUT_DIRECTORY\\|LIBRARY_OUTPUT_DIRECTORY_\\w+\\|LIBRARY_OUTPUT_NAME\\|LIBRARY_OUTPUT_NAME_\\w+\\|LINKER_LANGUAGE\\|LINK_DEPENDS\\|LINK_FLAGS\\|LINK_FLAGS_\\w+\\|LINK_INTERFACE_LIBRARIES\\|LINK_INTERFACE_LIBRARIES_\\w+\\|LINK_INTERFACE_MULTIPLICITY\\|LINK_INTERFACE_MULTIPLICITY_\\w+\\|LINK_SEARCH_END_STATIC\\|LOCATION\\|LOCATION_\\w+\\|MACOSX_BUNDLE\\|MACOSX_BUNDLE_INFO_PLIST\\|MACOSX_FRAMEWORK_INFO_PLIST\\|MAP_IMPORTED_CONFIG_\\w+\\|OSX_ARCHITECTURES\\|OSX_ARCHITECTURES_\\w+\\|OUTPUT_NAME\\|OUTPUT_NAME_\\w+\\|POST_INSTALL_SCRIPT\\|PREFIX\\|PRE_INSTALL_SCRIPT\\|PRIVATE_HEADER\\|PROJECT_LABEL\\|PUBLIC_HEADER\\|RESOURCE\\|RULE_LAUNCH_COMPILE\\|RULE_LAUNCH_CUSTOM\\|RULE_LAUNCH_LINK\\|RUNTIME_OUTPUT_DIRECTORY\\|RUNTIME_OUTPUT_DIRECTORY_\\w+\\|RUNTIME_OUTPUT_NAME\\|RUNTIME_OUTPUT_NAME_\\w+\\|SKIP_BUILD_RPATH\\|SOURCES\\|SOVERSION\\|STATIC_LIBRARY_FLAGS\\|STATIC_LIBRARY_FLAGS_\\w+\\|SUFFIX\\|TYPE\\|VERSION\\|VS_KEYWORD\\|VS_SCC_LOCALPATH\\|VS_SCC_PROJECTNAME\\|VS_SCC_PROVIDER\\|WIN32_EXECUTABLE\\|XCODE_ATTRIBUTE_\\w+\\|ATTACHED_FILES\\|ATTACHED_FILES_ON_FAIL\\|COST\\|DEPENDS\\|ENVIRONMENT\\|FAIL_REGULAR_EXPRESSION\\|LABELS\\|MEASUREMENT\\|PASS_REGULAR_EXPRESSION\\|PROCESSORS\\|REQUIRED_FILES\\|RESOURCE_LOCK\\|RUN_SERIAL\\|TIMEOUT\\|WILL_FAIL\\|WORKING_DIRECTORY\\|ABSTRACT\\|COMPILE_DEFINITIONS\\|COMPILE_DEFINITIONS_\\w+\\|COMPILE_FLAGS\\|EXTERNAL_OBJECT\\|GENERATED\\|HEADER_FILE_ONLY\\|KEEP_EXTENSION\\|LABELS\\|LANGUAGE\\|LOCATION\\|MACOSX_PACKAGE_LOCATION\\|OBJECT_DEPENDS\\|OBJECT_OUTPUTS\\|SYMBOLIC\\|WRAP_EXCLUDE\\|ADVANCED\\|HELPSTRING\\|MODIFIED\\|STRINGS\\|TYPE\\|VALUE\\|COMMAND\\|STREQUAL\\|MATCHES\\|REGEX\\|REPLACE\\|STATUS\\|WARNING\\|FATAL_ERROR\\|TOLOWER\\|READ\\|APPEND\\|MATCH\\|TEMP\\|PARENT_SCOPE\\|TRUE\\|FALSE\\|NAME\\|PATH\\|NOT\\|$ENV\\|build_name\\|exec_program\\|export_library_dependencies\\|install_files\\|install_programs\\|install_targets\\|link_libraries\\|make_directory\\|remove\\|subdir_depends\\|subdirs\\|use_mangled_mesa\\|utility_source\\|variable_requires\\|write_file\\|AddFileDependencies\\|BundleUtilities\\|CMakeBackwardCompatibilityCXX\\|CMakeDependentOption\\|CMakeDetermineVSServicePack\\|CMakeFindFrameworks\\|CMakeForceCompiler\\|CMakeParseArguments\\|CMakePrintSystemInformation\\|CMakeVerifyManifest\\|CPack\\|CPackDeb\\|CPackRPM\\|CTest\\|CTestScriptMode\\|CheckCCompilerFlag\\|CheckCSourceCompiles\\|CheckCSourceRuns\\|CheckCXXCompilerFlag\\|CheckCXXSourceCompiles\\|CheckCXXSourceRuns\\|CheckFortranFunctionExists\\|CheckFunctionExists\\|CheckIncludeFile\\|CheckIncludeFileCXX\\|CheckIncludeFiles\\|CheckLibraryExists\\|CheckStructHasMember\\|CheckSymbolExists\\|CheckTypeSize\\|CheckVariableExists\\|Dart\\|Documentation\\|ExternalProject\\|FeatureSummary\\|FindALSA\\|FindASPELL\\|FindAVIFile\\|FindBISON\\|FindBLAS\\|FindBZip2\\|FindBoost\\|FindBullet\\|FindCABLE\\|FindCUDA\\|FindCURL\\|FindCVS\\|FindCoin3D\\|FindCups\\|FindCurses\\|FindCxxTest\\|FindCygwin\\|FindDCMTK\\|FindDart\\|FindDevIL\\|FindDoxygen\\|FindEXPAT\\|FindFLEX\\|FindFLTK\\|FindFLTK2\\|FindFreetype\\|FindGCCXML\\|FindGDAL\\|FindGIF\\|FindGLUT\\|FindGTK\\|FindGTK2\\|FindGTest\\|FindGettext\\|FindGit\\|FindGnuTLS\\|FindGnuplot\\|FindHDF5\\|FindHSPELL\\|FindHTMLHelp\\|FindITK\\|FindImageMagick\\|FindJNI\\|FindJPEG\\|FindJasper\\|FindJava\\|FindKDE3\\|FindKDE4\\|FindLAPACK\\|FindLATEX\\|FindLibArchive\\|FindLibXml2\\|FindLibXslt\\|FindLua50\\|FindLua51\\|FindMFC\\|FindMPEG\\|FindMPEG2\\|FindMPI\\|FindMatlab\\|FindMotif\\|FindOpenAL\\|FindOpenGL\\|FindOpenMP\\|FindOpenSSL\\|FindOpenSceneGraph\\|FindOpenThreads\\|FindPHP4\\|FindPNG\\|FindPackageHandleStandardArgs\\|FindPackageMessage\\|FindPerl\\|FindPerlLibs\\|FindPhysFS\\|FindPike\\|FindPkgConfig\\|FindPostgreSQL\\|FindProducer\\|FindProtobuf\\|FindPythonInterp\\|FindPythonLibs\\|FindQt\\|FindQt3\\|FindQt4\\|FindQuickTime\\|FindRTI\\|FindRuby\\|FindSDL\\|FindSDL_image\\|FindSDL_mixer\\|FindSDL_net\\|FindSDL_sound\\|FindSDL_ttf\\|FindSWIG\\|FindSelfPackers\\|FindSquish\\|FindSubversion\\|FindTCL\\|FindTIFF\\|FindTclStub\\|FindTclsh\\|FindThreads\\|FindUnixCommands\\|FindVTK\\|FindWget\\|FindWish\\|FindX11\\|FindXMLRPC\\|FindZLIB\\|Findosg\\|FindosgAnimation\\|FindosgDB\\|FindosgFX\\|FindosgGA\\|FindosgIntrospection\\|FindosgManipulator\\|FindosgParticle\\|FindosgProducer\\|FindosgShadow\\|FindosgSim\\|FindosgTerrain\\|FindosgText\\|FindosgUtil\\|FindosgViewer\\|FindosgVolume\\|FindosgWidget\\|Findosg_functions\\|FindwxWidgets\\|FindwxWindows\\|FortranCInterface\\|GetPrerequisites\\|InstallRequiredSystemLibraries\\|MacroAddFileDependencies\\|Qt4ConfigDependentSettings\\|Qt4Macros\\|SelectLibraryConfigurations\\|SquishTestScript\\|TestBigEndian\\|TestCXXAcceptsFlag\\|TestForANSIForScope\\|TestForANSIStreamHeaders\\|TestForSSTREAM\\|TestForSTDNamespace\\|UseEcos\\|UsePkgConfig\\|UseQt4\\|UseSWIG\\|Use_wxWindows\\|UsewxWidgets\\|CMP0000\\|CMP0001\\|CMP0002\\|CMP0003\\|CMP0004\\|CMP0005\\|CMP0006\\|CMP0007\\|CMP0008\\|CMP0009\\|CMP0010\\|CMP0011\\|CMP0012\\|CMP0013\\|CMP0014\\|CMP0015\\|CMP0016\\|CMP0017\\|BUILD_SHARED_LIBS\\|CMAKE_BACKWARDS_COMPATIBILITY\\|CMAKE_BUILD_TYPE\\|CMAKE_COLOR_MAKEFILE\\|CMAKE_CONFIGURATION_TYPES\\|CMAKE_FIND_LIBRARY_PREFIXES\\|CMAKE_FIND_LIBRARY_SUFFIXES\\|CMAKE_IGNORE_PATH\\|CMAKE_INCLUDE_PATH\\|CMAKE_INSTALL_PREFIX\\|CMAKE_LIBRARY_PATH\\|CMAKE_MFC_FLAG\\|CMAKE_MODULE_PATH\\|CMAKE_NOT_USING_CONFIG_FLAGS\\|CMAKE_POLICY_DEFAULT_CMP\\w+\\|CMAKE_PREFIX_PATH\\|CMAKE_PROGRAM_PATH\\|CMAKE_SKIP_INSTALL_ALL_DEPENDENCY\\|CMAKE_SYSTEM_IGNORE_PATH\\|CMAKE_SYSTEM_INCLUDE_PATH\\|CMAKE_SYSTEM_LIBRARY_PATH\\|CMAKE_SYSTEM_PREFIX_PATH\\|CMAKE_SYSTEM_PROGRAM_PATH\\|CMAKE_USER_MAKE_RULES_OVERRIDE\\|APPLE\\|BORLAND\\|CMAKE_CL_64\\|CMAKE_COMPILER_2005\\|CMAKE_HOST_APPLE\\|CMAKE_HOST_SYSTEM\\|CMAKE_HOST_SYSTEM_NAME\\|CMAKE_HOST_SYSTEM_PROCESSOR\\|CMAKE_HOST_SYSTEM_VERSION\\|CMAKE_HOST_UNIX\\|CMAKE_HOST_WIN32\\|CMAKE_OBJECT_PATH_MAX\\|CMAKE_SYSTEM\\|CMAKE_SYSTEM_NAME\\|CMAKE_SYSTEM_PROCESSOR\\|CMAKE_SYSTEM_VERSION\\|CYGWIN\\|MSVC\\|MSVC80\\|MSVC_IDE\\|MSVC_VERSION\\|UNIX\\|WIN32\\|XCODE_VERSION\\|CMAKE_\\w+_ARCHIVE_APPEND\\|CMAKE_\\w+_ARCHIVE_CREATE\\|CMAKE_\\w+_ARCHIVE_FINISH\\|CMAKE_\\w+_COMPILER\\|CMAKE_\\w+_COMPILER_ABI\\|CMAKE_\\w+_COMPILER_ID\\|CMAKE_\\w+_COMPILER_LOADED\\|CMAKE_\\w+_COMPILE_OBJECT\\|CMAKE_\\w+_CREATE_SHARED_LIBRARY\\|CMAKE_\\w+_CREATE_SHARED_MODULE\\|CMAKE_\\w+_CREATE_STATIC_LIBRARY\\|CMAKE_\\w+_FLAGS_DEBUG\\|CMAKE_\\w+_FLAGS_MINSIZEREL\\|CMAKE_\\w+_FLAGS_RELEASE\\|CMAKE_\\w+_FLAGS_RELWITHDEBINFO\\|CMAKE_\\w+_IGNORE_EXTENSIONS\\|CMAKE_\\w+_IMPLICIT_INCLUDE_DIRECTORIES\\|CMAKE_\\w+_IMPLICIT_LINK_DIRECTORIES\\|CMAKE_\\w+_IMPLICIT_LINK_LIBRARIES\\|CMAKE_\\w+_LINKER_PREFERENCE\\|CMAKE_\\w+_LINKER_PREFERENCE_PROPAGATES\\|CMAKE_\\w+_LINK_EXECUTABLE\\|CMAKE_\\w+_OUTPUT_EXTENSION\\|CMAKE_\\w+_PLATFORM_ID\\|CMAKE_\\w+_SIZEOF_DATA_PTR\\|CMAKE_\\w+_SOURCE_FILE_EXTENSIONS\\|CMAKE_COMPILER_IS_GNU\\w+\\|CMAKE_Fortran_MODDIR_DEFAULT\\|CMAKE_Fortran_MODDIR_FLAG\\|CMAKE_Fortran_MODOUT_FLAG\\|CMAKE_INTERNAL_PLATFORM_ABI\\|CMAKE_USER_MAKE_RULES_OVERRIDE_\\w+\\|CMAKE_\\w+_POSTFIX\\|CMAKE_ARCHIVE_OUTPUT_DIRECTORY\\|CMAKE_BUILD_WITH_INSTALL_RPATH\\|CMAKE_DEBUG_POSTFIX\\|CMAKE_EXE_LINKER_FLAGS\\|CMAKE_EXE_LINKER_FLAGS_\\w+\\|CMAKE_Fortran_MODULE_DIRECTORY\\|CMAKE_INCLUDE_CURRENT_DIR\\|CMAKE_INSTALL_NAME_DIR\\|CMAKE_INSTALL_RPATH\\|CMAKE_INSTALL_RPATH_USE_LINK_PATH\\|CMAKE_LIBRARY_OUTPUT_DIRECTORY\\|CMAKE_LIBRARY_PATH_FLAG\\|CMAKE_LINK_DEF_FILE_FLAG\\|CMAKE_LINK_LIBRARY_FILE_FLAG\\|CMAKE_LINK_LIBRARY_FLAG\\|CMAKE_NO_BUILTIN_CHRPATH\\|CMAKE_RUNTIME_OUTPUT_DIRECTORY\\|CMAKE_SKIP_BUILD_RPATH\\|CMAKE_TRY_COMPILE_CONFIGURATION\\|CMAKE_USE_RELATIVE_PATHS\\|EXECUTABLE_OUTPUT_PATH\\|LIBRARY_OUTPUT_PATH\\|CMAKE_AR\\|CMAKE_BINARY_DIR\\|CMAKE_BUILD_TOOL\\|CMAKE_CACHEFILE_DIR\\|CMAKE_CACHE_MAJOR_VERSION\\|CMAKE_CACHE_MINOR_VERSION\\|CMAKE_CACHE_PATCH_VERSION\\|CMAKE_CFG_INTDIR\\|CMAKE_COMMAND\\|CMAKE_CROSSCOMPILING\\|CMAKE_CTEST_COMMAND\\|CMAKE_CURRENT_BINARY_DIR\\|CMAKE_CURRENT_LIST_DIR\\|CMAKE_CURRENT_LIST_FILE\\|CMAKE_CURRENT_LIST_LINE\\|CMAKE_CURRENT_SOURCE_DIR\\|CMAKE_DL_LIBS\\|CMAKE_EDIT_COMMAND\\|CMAKE_EXECUTABLE_SUFFIX\\|CMAKE_EXTRA_GENERATOR\\|CMAKE_EXTRA_SHARED_LIBRARY_SUFFIXES\\|CMAKE_GENERATOR\\|CMAKE_HOME_DIRECTORY\\|CMAKE_IMPORT_LIBRARY_PREFIX\\|CMAKE_IMPORT_LIBRARY_SUFFIX\\|CMAKE_LINK_LIBRARY_SUFFIX\\|CMAKE_MAJOR_VERSION\\|CMAKE_MAKE_PROGRAM\\|CMAKE_MINOR_VERSION\\|CMAKE_PARENT_LIST_FILE\\|CMAKE_PATCH_VERSION\\|CMAKE_PROJECT_NAME\\|CMAKE_RANLIB\\|CMAKE_ROOT\\|CMAKE_SHARED_LIBRARY_PREFIX\\|CMAKE_SHARED_LIBRARY_SUFFIX\\|CMAKE_SHARED_MODULE_PREFIX\\|CMAKE_SHARED_MODULE_SUFFIX\\|CMAKE_SIZEOF_VOID_P\\|CMAKE_SKIP_RPATH\\|CMAKE_SOURCE_DIR\\|CMAKE_STANDARD_LIBRARIES\\|CMAKE_STATIC_LIBRARY_PREFIX\\|CMAKE_STATIC_LIBRARY_SUFFIX\\|CMAKE_TWEAK_VERSION\\|CMAKE_USING_VC_FREE_TOOLS\\|CMAKE_VERBOSE_MAKEFILE\\|CMAKE_VERSION\\|PROJECT_BINARY_DIR\\|PROJECT_NAME\\|PROJECT_SOURCE_DIR\\|\\w+_BINARY_DIR\\|\\w+_SOURCE_DIR\\)\\b" 1 font-lock-type-face)
        '("\\b[A-Z][A-Z0-9_]*\\b" 0 font-lock-constant-face)
        '("`[^`]*`" 0 font-lock-string-face)
        )
  "Highlighting expressions for CMAKE mode."
  )

;------------------------------------------------------------------------------

;;
;; Syntax table for this mode.  Initialize to nil so that it is
;; regenerated when the cmake-mode function is called.
;;
(defvar cmake-mode-syntax-table nil "Syntax table for cmake-mode.")
(setq cmake-mode-syntax-table nil)

;;
;; User hook entry point.
;;
(defvar cmake-mode-hook nil)

;;
;; Indentation increment.
;;
(defvar cmake-tab-width 2)

;------------------------------------------------------------------------------

;;
;; CMake mode startup function.
;;
(defun cmake-mode ()
  "Major mode for editing CMake listfiles."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cmake-mode)
  (setq mode-name "CMAKE")

  ; Create the syntax table
  (setq cmake-mode-syntax-table (make-syntax-table))
  (set-syntax-table cmake-mode-syntax-table)
  (modify-syntax-entry ?_  "w" cmake-mode-syntax-table)
  (modify-syntax-entry ?\(  "()" cmake-mode-syntax-table)
  (modify-syntax-entry ?\)  ")(" cmake-mode-syntax-table)
  (modify-syntax-entry ?# "<" cmake-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cmake-mode-syntax-table)

  ; Setup font-lock mode.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cmake-font-lock-keywords))

  ; Setup indentation function.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cmake-indent)

  ; Setup comment syntax.
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ; Run user hooks.
  (run-hooks 'cmake-mode-hook))

; Help mode starts here


(defun cmake-command-run (type &optional topic)
  "Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list."
  (interactive "s")
  (let* ((bufname (concat "*CMake" type (if topic "-") topic "*"))
         (buffer  (get-buffer bufname))
         )
    (if buffer
        (display-buffer buffer 'not-this-window)
      ;; Buffer doesn't exist.  Create it and fill it
      (setq buffer (generate-new-buffer bufname))
      (setq command (concat cmake-mode-cmake-executable " " type " " topic))
      (message "Running %s" command)
      ;; We don't want the contents of the shell-command running to the
      ;; minibuffer, so turn it off.  A value of nil means don't automatically
      ;; resize mini-windows.
      (setq resize-mini-windows-save resize-mini-windows)
      (setq resize-mini-windows nil)
      (shell-command command buffer)
      ;; Save the original window, so that we can come back to it later.
      ;; save-excursion doesn't seem to work for this.
      (setq window (selected-window))
      ;; We need to select it so that we can apply special modes to it
      (select-window (display-buffer buffer 'not-this-window))
      (cmake-mode)
      (toggle-read-only t)
      ;; Restore the original window
      (select-window window)
      (setq resize-mini-windows resize-mini-windows-save)
      )
    )
  )

(defun cmake-help-list-commands ()
  "Prints out a list of the cmake commands."
  (interactive)
  (cmake-command-run "--help-command-list")
  )

(defvar cmake-help-command-history nil "Topic read history.")

(require 'thingatpt)
(defun cmake-get-topic (type)
  "Gets the topic from the minibuffer input.  The default is the word the cursor is on."
  (interactive)
  (let* ((default-entry (word-at-point))
         (input (read-string
                 (format "CMake %s (default %s): " type default-entry) ; prompt
                 nil ; initial input
                 'cmake-help-command-history ; command history
                 default-entry ; default-value
                 )))
    (if (string= input "")
        (error "No argument given")
      input))
  )


(defun cmake-help-command ()
  "Prints out the help message corresponding to the command the cursor is on."
  (interactive)
  (setq command (cmake-get-topic "command"))
  (cmake-command-run "--help-command" (downcase command))
  )


; This file provides cmake-mode.
(provide 'cmake-mode)

;;; cmake-mode.el ends here
